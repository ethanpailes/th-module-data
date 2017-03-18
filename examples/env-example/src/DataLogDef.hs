{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}

module DataLogDef where

import Data.Serialize
import Data.Typeable
import GHC.Generics
import Env

data EnvValue = EnvValue String
  deriving(Generic, Typeable, Eq, Ord, Show)
instance Serialize EnvValue

$(genStringEnv "th_module_data_env_test" ''EnvValue)



