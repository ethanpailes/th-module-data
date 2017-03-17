{-# LANGUAGE TemplateHaskell #-}

-- | environments implimented in terms of `DataLog`

module Env (genStringEnv, genEnv, genEnv') where

import Language.Haskell.TH (Q, Name, Dec(..))
import Language.Haskell.TH.Syntax (Type(..), mkName,
                                   Exp(..), Pat(..), Body(..))
import DataLog (genDataLog', mkDataLogGetName, mkDataLogAppendName)
import qualified Data.Vector as V


-- | given a namespace and a type to be stored in the environment, a,
--   this generates two functions:
--
--   `envPut_<namespace>` :: String -> a -> Q ()
--
--   and
--
--   `envGet_<namespace>` :: String -> Q a
--
--   which manipulate data log for the given namespace in the obvious
--   way. Note that `envGet_<namespace>` is O(n).
genStringEnv :: String -- ^ the namespace
             -> Name -- ^ the value type
             -> Q [Dec]
genStringEnv namespace valueType = genEnv namespace ''String valueType

-- | Like `genStringEnv` except that you may choose any key type so
--   long as it impliments `Eq`
genEnv :: String -- ^ the namespace
       -> Name -- ^ the key type
       -> Name -- ^ the value type
       -> Q [Dec]
genEnv namespace t1 t2 = genEnv' namespace t1 t2 [''Eq, ''Ord, ''Show]


-- | Like `genStringEnv` except that you may specify the
--   set of typeclasses to try to derive on the underlying
--   log type.
genEnv' :: String -- ^ the namespace
        -> Name -- ^ the name of the key type
        -> Name -- ^ the name of the value type
        -> [Name] -- ^ the list of extra typeclasses to derive
        -> Q [Dec]
genEnv' namespace keyTyName valueTyName extraDerives = do
  let tupTyName = mkName $ "EnvTypTy_" ++ namespace
      tupTy = TySynD tupTyName [] (AppT (AppT (TupleT 2)
                                   (ConT keyTyName)) (ConT valueTyName))

  envGet <- genEnvGet namespace
  envPut <- genEnvPut namespace

  dataLogDecs <- genDataLog' namespace tupTyName extraDerives

  return $ tupTy : envPut : envGet : dataLogDecs

genEnvGet :: String -- ^ the namespace
          -> Q Dec
genEnvGet namespace = do
  let dlGetName = mkDataLogGetName namespace
  body <- [| \key -> do
       dl <- $(return . VarE $ dlGetName)
       key `vectorLookup` dl
    |]
  return $ ValD (VarP (mkName $ "envGet_" ++ namespace)) (NormalB body) []

genEnvPut :: String -- ^ the namespace
          -> Q Dec
genEnvPut namespace = do
  let dlAppendName = mkDataLogAppendName namespace
  body <- [| \key value -> $(return . VarE $ dlAppendName) (key, value) |]
  return $ ValD (VarP (mkName $ "envPut_" ++ namespace)) (NormalB body) []


vectorLookup :: Eq a => a -> V.Vector (a, b) -> Maybe b
vectorLookup k v =
  case V.length v of
    0 -> Nothing
    n ->
      let hd = V.head v in
      if k == fst hd
          then Just . snd $ hd
          else vectorLookup k (V.slice 1 n v)



