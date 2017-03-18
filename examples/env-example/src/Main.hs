{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}

module Main where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax as THS
import ParentModule()
import DataLogDef
import System.Exit
import Control.Monad (when)

$(dataLogInit_th_module_data_env_test >> return [])
$(do { gp <- envGet_th_module_data_env_test "gp_k"
     ; p <-  envGet_th_module_data_env_test "p_k"
     ; nothing <- envGet_th_module_data_env_test "not_a_key"
     ; let ok = gp == Just (EnvValue "gp_v")
              && p == Just (EnvValue "p_v")
              && nothing == Nothing
     ; okExp <- THS.lift $ ok

     ; when (not ok) $
         runIO . putStrLn $ "[ FAIL ] env test failed. gp="
                             ++ show gp
                             ++ " p=" ++ show p
                             ++ " nothing=" ++ show nothing

     ; return [ValD (VarP (mkName "testEnv")) (NormalB okExp) []]
     })

main :: IO ()
main = exitWith (if testEnv then ExitSuccess else ExitFailure 1)
