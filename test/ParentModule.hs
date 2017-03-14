{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}

module ParentModule where

import DataLogDef
import Language.Haskell.TH
import GrandParentModule()

$(dataLogInit_th_module_data_test >> return [])

$(dataLogAppend_th_module_data_test (LogContent 3 "p3") >> return [])

-- this should not overwrite the log
$(dataLogInit_th_module_data_test >> return [])

$(dataLogAppend_th_module_data_test (LogContent 4 "p4") >> return [])

$(dataLogGet_th_module_data_test >>= \log -> do
     runIO $ print log
     return [])
