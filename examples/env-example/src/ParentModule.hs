{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}

module ParentModule where

import DataLogDef
import GrandParentModule()

--
-- Env
--


$(dataLogInit_th_module_data_env_test >> return [])
$(envPut_th_module_data_env_test "p_k" (EnvValue "p_v") >> return [])
