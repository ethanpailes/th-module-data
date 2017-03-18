{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Main where

import Turtle
import Turtle.Prelude
import Prelude hiding (FilePath)
import System.IO.Temp (withSystemTempDirectory)
import Filesystem.Path.CurrentOS (decodeString, encode, toText)
import Control.Monad

-- who ever said scripts shoudl be safe?
tt :: FilePath -> Text
tt f = fp
  where Right fp = toText f

main :: IO ()
main = withSystemTempDirectory "tmd-test" $ \(decodeString -> dir) -> sh $ do
  proc "cp" ["-r", ".", tt dir] empty

  --
  -- run the checks, then modify different modules to force
  -- odd recompile patterns
  --
  let dlSrc = dir </> "examples" </> "data-log-example" </> "src"
  let envSrc = dir </> "examples" </> "env-example" </> "src"
  checkAfterMods (checkAll dir) $ [
      do { append (dlSrc </> "GrandParentModule.hs") (return "-- a comment")
         ; append (envSrc </> "GrandParentModule.hs") (return "-- a comment")
         ; return ()
         }
    , do { append (dlSrc </> "ParentModule.hs") (return "-- a comment")
         ; append (envSrc </> "ParentModule.hs") (return "-- a comment")
         ; return ()
         }
    , do { append (dlSrc </> "Main.hs") (return "-- a comment")
         ; append (envSrc </> "Main.hs") (return "-- a comment")
         ; return ()
         }
    ]


checkAfterMods :: Shell ExitCode -> [Shell ()] -> Shell ExitCode
checkAfterMods check [] = check
checkAfterMods check (m:ms) = do
  m
  e <- check
  case e of
    ExitSuccess -> checkAfterMods check ms
    ef -> return ef


checkAll :: FilePath -> Shell ExitCode
checkAll tmpDir = do
  dl <- checkExample tmpDir "data-log-example"
  env <- checkExample tmpDir "env-example"
  case (dl, env) of
    (ExitSuccess, ExitSuccess) -> return ExitSuccess
    _ -> return $ ExitFailure 1


checkExample :: FilePath -> FilePath -> Shell ExitCode
checkExample tmpDir exampleDir = do
  cd $ tmpDir </> "examples" </> exampleDir
  build <- proc "stack" ["build"] empty
  run <- proc "stack" ["exec", tt exampleDir] empty
  case (build, run) of
    (ExitSuccess, ExitSuccess) -> return ExitSuccess
    _ -> do
      printf ("Check failed for example "%s) (tt exampleDir)
      return $ ExitFailure 1



