{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax as THS
import ParentModule()
import DataLogDef
import qualified Data.Vector as V
import System.Exit
import Control.Monad (when)

$(dataLogInit_th_module_data_test >> return [])
$(dataLogGet_th_module_data_test >>= \log -> do
     let expected = V.fromList [
              -- grand parent module
                LogContent 1 "gp1"
              , LogContent 2 "gp2"

              -- parent module
              , LogContent 3 "p3"
              , LogContent 4 "p4"
              ]
     let ok = log == expected
     okExp <- THS.lift ok

     when (not ok) $
       runIO $ putStrLn $ "[ FAIL ] data log test failed. log=" ++ show log

     return [ValD (VarP (mkName "testDataLog")) (NormalB okExp) []])

main :: IO ()
main = exitWith (if testDataLog then ExitSuccess else ExitFailure 1)
