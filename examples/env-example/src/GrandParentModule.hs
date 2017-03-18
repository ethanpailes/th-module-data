{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}

module GrandParentModule where

import DataLogDef

$(envPut_th_module_data_env_test "gp_k" (EnvValue "gp_v") >> return [])

