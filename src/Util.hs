
module Util where

import Data.Vector (Vector, empty, snoc)
import Language.Haskell.TH.Syntax (mkName, Name)

-- | execute the given monadic action (given a state continuation)
--   until it returns `Nothing`, accumlating the results in a vector.
--   For now this is O(n^2), but it should be very possible to do
--   in O(n) time with some lower level programming.
createM :: Monad m => b -> (b -> m (Maybe (a, b))) -> m (Vector a)
createM st a = loop st empty
  where loop currentSt vec = do
          v <- a currentSt
          case v of
            Just (elem, newSt) -> loop newSt (vec `snoc` elem)
            Nothing -> return vec

serialisedIntLength :: Int
serialisedIntLength = 8

mkDataLogTyName :: String -> Name
mkDataLogTyName namespace = mkName $ "DataLogFor_" ++ namespace

mkDataLogInitName :: String -> Name
mkDataLogInitName namespace = mkName $ "dataLogInit_" ++ namespace


mkDataLogAppendName :: String -> Name
mkDataLogAppendName namespace = mkName $ "dataLogAppend_" ++ namespace

mkDataLogGetName :: String -> Name
mkDataLogGetName namespace = mkName $ "dataLogGet_" ++ namespace
