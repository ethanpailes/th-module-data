{-# LANGUAGE DeriveGeneric, DefaultSignatures, DeriveDataTypeable,
             TemplateHaskell #-}
{-|
Module      : DataLog
Description : Provides TH functions to add a stateful log to the Q monad
Copyright   : (c) Ethan Pailes, 2017
License     : BSD3
Maintainer  : ethanpailes@email.com
Stability   : experimental
Portability : POSIX

The `genDataLog` and `genDataLog'` functions provide the ability
to generate a stateful log for the Q monad in order to support
the need for type environments in DSLs.
-}

module DataLog (genDataLog, genDataLog') where

import Util (createM, mkDataLogGetName, mkDataLogAppendName,
             mkDataLogInitName, mkDataLogTyName, serialisedIntLength)
import Data.Serialize (Serialize(..), encode, decode)
import Language.Haskell.TH (Q, thisModule, runIO, newName)
import Language.Haskell.TH.Syntax (Module(Module), qGetQ, qPutQ
                                  , PkgName(PkgName), ModName(ModName)
                                  , mkName
                                  , Name
                                  , Match(Match)
                                  , Exp(..)
                                  , Type(..)
                                  , Dec(..)
                                  , Con(..)
                                  , Stmt(..)
                                  , Strict(..)
                                  , Pat(..)
                                  , Body(..)
                                  )
import qualified Language.Haskell.TH.Syntax as THS
import System.IO (IOMode(AppendMode, ReadMode, WriteMode))
import Data.ByteString (hPut, hGetContents)
import qualified Data.ByteString as BS
import File (withModuleDataFile, dataFilePath)
import Data.Vector (Vector, snoc)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import Control.Monad (when)
import Data.Typeable (Typeable)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))

-- | Indicates if the modules depends on other modules, and if so
--   points in the right direction.
data Deps = Deps [Module]
          deriving(Generic, Show)

-- orphan instance, but there is no way to cleanly serialize
-- `Deps` without this guy. A solution would be for Module
-- to derive Generic.
instance Serialize Module where
  put (Module (PkgName pkg) (ModName mod)) = do
    put pkg
    put mod
  get = do
    pkg <- get
    mod <- get
    return $ Module (PkgName pkg) (ModName mod)

instance Serialize Deps where
  put (Deps mods) = put mods
  get = Deps <$> get

-- | generate a type named `DataLogFor_<namespace>` which just wraps vector,
--   a value of type `Q ()` named `dataLogInit_<namespace>` which reads
--   all the data files for the deps into the log type and stores it in the
--   Q monad's module state, and a value of type
--   `Typeable a, Serialize a => a -> Q ()` called
--   `dataLogAppend_<namespace>` which allows you to append to the
--   data log.
--
--   The content type parameter indicates the type of the thing in the log.
--   It must have `Serialize` and `Typeable` instances.
--
--   Requires `-XDeriveDataTypeable`
genDataLog :: String -- ^ the namespace
           -> Name -- ^ the content type
           -> Q [Dec]
genDataLog namespace tyName =
  genDataLog' namespace tyName [''Eq, ''Ord, ''Show]


-- | like `genDataLog` except that it allows you to specify the list of
--   extra typeclasses to add to the deriving statement explicitly. If nothing
--   is specified it will just derive `Typeable`, which is requried in order
--   to store the value in the Q Monad. `genDataLog` derives `Eq`, `Ord`,
--   and `Show` by default.
genDataLog' :: String -- ^ the namespace
            -> Name -- ^ the content type
            -> [Name] -- ^ a list of extra typeclasses to add to the derive statement
            -> Q [Dec]
genDataLog' namespace contentTy extraDerives = do
  logTy <- genLogTy namespace contentTy extraDerives
  dataLogInit <- genDataLogInit namespace contentTy extraDerives
  dataLogAppend <- genDataLogAppend namespace contentTy extraDerives
  dataLogGet <- genDataLogGet namespace contentTy extraDerives

  return [ logTy
         , dataLogInit
         , dataLogAppend
         , dataLogGet
         ]

-- | generate the datatype used for the in-memory log
genLogTy :: String -- ^ the namespace
         -> Name -- ^ the content type
         -> [Name] -- ^ the extra derives
         -> Q Dec
genLogTy namespace contentTy extraDerives = do
  let name = mkDataLogTyName namespace
  return $ DataD [] name []
                [NormalC name
                 [(NotStrict, AppT (ConT ''Vector) (ConT contentTy))]]
                 (''Typeable : extraDerives)

-- | generate the init funciton which slurps all the data files
--   into the in-memory log, and is idempotent so that it can
--   be safely called every time a quasi quoter is invoked.
genDataLogInit :: String -- ^ the namespace
               -> Name -- ^ the content type
               -> [Name] -- ^ the extra derives
               -> Q Dec
genDataLogInit namespace contentTy extraDerives = do
  let name = mkDataLogInitName namespace
      logTyName = mkDataLogTyName namespace
  body <- [| do
      newMod <- newModuleCheckAndSet $(THS.lift namespace)
      when newMod $ do
        log <- readDepLogs namespace :: Q (Vector $(return . ConT $ contentTy))
        qPutQ ($(return . ConE $ logTyName) log)
    |]
  return $ ValD (VarP name) (NormalB body) []

-- | Generate the append function. I bet you can't guess what it does.
genDataLogAppend :: String -- ^ the namespace
                 -> Name -- ^ the content type
                 -> [Name] -- ^ the extra derives
                 -> Q Dec
genDataLogAppend namespace contentTy extraDerives = do
  let name = mkDataLogAppendName namespace
      logTyName = mkDataLogTyName namespace
      appendCase log datum =
        logCase namespace log (appendExists datum) (appendNew datum)

      appendExists datum l =
        AppE (VarE 'qPutQ) (ParensE
                            (AppE (ConE logTyName)
                            (UInfixE (VarE l) (VarE 'snoc) (VarE datum))))
      appendNew datum =
        AppE (VarE 'qPutQ) (ParensE (AppE (ConE logTyName)
                                     (VarE 'V.singleton `AppE` VarE datum) ))
  body <- do
    logName <- newName "log"
    datumName <- newName "datum"
    body <- [| do
        mod <- thisModule
        newMod <- newModuleCheckAndSet namespace
        let mode = if newMod then WriteMode else AppendMode
        let blob = encode $(return . VarE $ datumName)
        withModuleDataFile mod namespace mode $ \h -> do
          -- if we have not already written the deps,
          -- then there are none for this file
          when newMod $ do
            let deps = encode $ Deps []
            runIO $ hPut h (encode (BS.length deps))
            runIO $ hPut h deps

          -- append to the in memory log
          $(return $ appendCase logName datumName)
          -- the code we want to generate is below.
          -- GHC does not like pattern splices, so we have to directly
          -- generate the whole case expression.
          -- log <- qGetQ :: Q (Mabye <LogTyName>)
          -- case log of
          --   Just (<LogTyName> l) -> qPutQ (<LogTyName> (l `snoc` datum))
          --   Nothing -> qPutQ (<LogTyName (V.singleton datum))

          -- append to the file
          runIO $ hPut h (encode (BS.length blob))
          runIO $ hPut h blob
      |]
    -- we need to directly construct the lambda to avoid the restriction
    -- on pattern splices
    return $ LamE [VarP datumName] body
  return $ ValD (VarP name) (NormalB body) []

-- | generate the get function which returns the whole log.
genDataLogGet :: String -- ^ the namespace
                 -> Name -- ^ the content type
                 -> [Name] -- ^ the extra derives
                 -> Q Dec
genDataLogGet namespace contentTy extraDerives = do
  let name = mkDataLogGetName namespace
  body <- do
    logName <- newName "log"
    datumName <- newName "datum"
    fail <- [| fail ("th-module-data:DataLog.hs:dataLogGet_"
                      ++ $(THS.lift namespace)
                      ++ " get called before init or append!") |]
    return $ logCase namespace logName
               (\l -> VarE 'return `AppE` VarE l)
               fail
  return $ ValD (VarP name) (NormalB body) []

logCase :: String -> Name -> (Name -> Exp) -> Exp -> Exp
logCase namespace log justCase nothingCase =
  let l = mkName "l"
      logTyName = mkDataLogTyName namespace
  in DoE [
          VarP log `BindS` (VarE 'qGetQ `SigE`
                            (ConT ''Q `AppT`
                             (ConT ''Maybe `AppT` ConT logTyName)))
        , NoBindS (CaseE (VarE log) [
             Match (ConP 'Just [ParensP (ConP logTyName [VarP l])])
               (NormalB (justCase l)) []
           , Match (ConP 'Nothing []) (NormalB nothingCase) []])
        ]


------------------------------------------------------------------------
--                                                                    --
-- Code to directly manipulate the log files and Q Monad module state --
--                                                                    --
-- In the below code the Q monad is used as an odd sort of IO         --
-- monad, rather than a way to generate template haskell.             --
--                                                                    --
------------------------------------------------------------------------



-- | fetch all the dependencies of this module, whether direct or transitive,
--   returning the results. Helper function for dataFileInit_<namespace>.
readDepLogs :: (Serialize a, Show a)
            => String -- ^ the namespace
            -> Q (Vector a)
readDepLogs namespace = do
  mod <- thisModule

  -- this function is only ever called the first time that
  -- dataFileInit_<namespace> is called for a given module.
  -- We want to overwrite any previous output, so we open the
  -- file in `WriteMode` rather than `AppendMode`
  withModuleDataFile mod namespace WriteMode $ \h -> do
    directLocalDeps <- thisModule >>= getModuleDeps namespace
    let deps = encode $ Deps directLocalDeps
    runIO $ hPut h (encode (BS.length deps))
    runIO $ hPut h deps

  -- construct the result
  let readLog mod = do
        (Deps deps, log) <- readModuleLog namespace mod
        depLogs <- mapM readLog deps
        return $ (V.concat depLogs) <> log
  directDeps <- getModuleDeps namespace mod
  V.concat <$> mapM readLog directDeps

-- | Uses `reifyModule` to get a list of imported modules which
--   have generated th-module-data files
getModuleDeps :: String -- ^ the namespace
              -> Module -- ^ the module
              -> Q [Module] -- ^ the deps
getModuleDeps namespace mdl = do
  THS.ModuleInfo importedMods <- THS.reifyModule mdl
  catMaybes <$> mapM ifExists importedMods
    where ifExists mod = do
            fileName <- dataFilePath mod namespace
            exists <- runIO $ doesFileExist fileName
            if exists
              then return . Just $ mod
              else return Nothing

-- | a list of all the th-module-data namespace which had
--   created a file for this module in this compilation run.
data ModuleDirtyFlags = ModuleDirtyFlags [String]
  deriving(Typeable, Show)

-- | Keeps a list of dirty flags for the modules that have been touched
--   in the current compilation unit. This is what allows the
--   generated `dataLogInit_<namespace>` functions to be idempotent
--   as we want.
newModuleCheckAndSet :: String -- ^ the namespace
                     -> Q Bool -- ^ true if this namespace is new for the module
newModuleCheckAndSet namespace = do
  dirtyFlags <- qGetQ
  case dirtyFlags of
    Nothing -> do
      qPutQ (ModuleDirtyFlags [namespace])
      return True
    Just (ModuleDirtyFlags flags) -> do
      if namespace `elem` flags
        then return False
        else do
          qPutQ (ModuleDirtyFlags (namespace:flags))
          return True

-- | Read the log for a give module into a vector
readModuleLog :: Serialize a
              => String -- ^ the namespace
              -> Module -- ^ the module to read
              -> Q (Deps, Vector a)
readModuleLog namespace mod = do
  withModuleDataFile mod namespace ReadMode $ \h -> do
    let checkedDecode src =
          case decode src of
            Left err ->
              fail $ "th-module-data:DataLog.hs cerial deserial error: " ++ err
            Right val -> return val

    source <- runIO $ hGetContents h

    -- parse the deps
    let (depsTag, rest) = BS.splitAt serialisedIntLength source
    depsLen <- checkedDecode depsTag :: Q Int
    deps <- checkedDecode rest :: Q Deps

    -- now parse the log
    let logSource = BS.drop depsLen rest
    log <- createM logSource $ \src ->
      if BS.null src then return Nothing else do
        let (len, blob) = BS.splitAt serialisedIntLength src
        blobLen <- checkedDecode len
        val <- checkedDecode blob
        return . Just $ (val, BS.drop (serialisedIntLength + blobLen) src)

    return (deps, log)

