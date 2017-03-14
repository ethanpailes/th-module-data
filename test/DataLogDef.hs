{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}

module DataLogDef where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Serialize
import Data.Typeable
import GHC.Generics
import DataLog
import Data.Vector as V

data LogContent = LogContent Int String
  deriving(Generic, Typeable, Eq, Ord, Show)
instance Serialize LogContent
$(genDataLog "th_module_data_test" ''LogContent)


-- DataLogFor_th_module_data_test
getLogContent :: Q (Vector LogContent)
getLogContent = do
  l <- qGetQ
  case l of
    Just (DataLogFor_th_module_data_test v) -> return v
    Nothing -> return empty


