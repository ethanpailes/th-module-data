{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}

module Lib where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import File
import DataLog
import Control.Monad
import Data.Vector
import Data.Typeable
import Data.Serialize
import GHC.Generics (Generic)
import System.IO
import Data.ByteString as BS
import Data.Vector as V


-- $(spliceIn (mkName "rootDir") baseDir)
-- $(spliceIn (mkName "thisM") thisM')

-- $writeFoo

data Content = Content Int String
  deriving(Generic, Typeable, Eq, Ord, Show)
instance Serialize Content

$(genDataLog "pads" ''Content)


appendDataLog :: Content -> Q ()
appendDataLog datum = do
  mod <- thisModule
  newMod <- newModuleCheckAndSet "pads"
  let blob = encode datum
  withModuleDataFile mod "pads" AppendMode $ \h -> do
    -- if we have not already written the deps, then there are none for this file
    when newMod $ do
      let deps = encode $ Deps []
      runIO $ hPut h (encode (BS.length deps))
      runIO $ hPut h deps

    -- append to the in memory log
    -- $(return $ appendCase logName datumName)
    -- the code we want to generate is below.
    -- GHC does not like pattern splices, so we have to directly
    -- generate the whole case expression.
    do { log <- qGetQ :: Q (Maybe DataLogFor_pads)
       ; case log of
          Just (DataLogFor_pads l) -> qPutQ (l `V.snoc` datum)
          Nothing -> qPutQ (DataLogFor_pads (V.singleton datum))
       }

    -- append to the file
    runIO $ hPut h (encode (BS.length blob))
    runIO $ hPut h blob

-- initBody :: Q ()
-- initBody = do
--   newMod <- newModuleCheckAndSet "pads"
--   when newMod $ do
--     log <- (readDepLogs "pads") :: Q (Vector Content)
--     qPutQ (DataLogFor_pads log)
-- $(writeThing (1 :: Int))
-- $(writeThing (2 :: Int))
-- $(writeThing (8 :: Int))
-- $(writeThing (10 :: Int))

-- $(readStuff (mkName "log"))

