{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}

module GrandParentModule where

import DataLogDef
import Language.Haskell.TH

$(dataLogInit_th_module_data_test >> return [])

$(dataLogAppend_th_module_data_test (LogContent 1 "gp1") >> return [])

-- this should not overwrite the log
$(dataLogInit_th_module_data_test >> return [])

$(dataLogAppend_th_module_data_test (LogContent 2 "gp2") >> return [])

-- uncomment to print the log at this point
-- $(dataLogGet_th_module_data_test >>= \log -> do
--      runIO $ print log
--      return [])
