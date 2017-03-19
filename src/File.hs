{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : File
Description : Utilities to manipulate th-module-data files
Copyright   : (c) Ethan Pailes, 2017
License     : BSD3
Maintainer  : ethanpailes@email.com
Stability   : experimental
Portability : POSIX

This supports finding the appropriate location to put build
data files depending on whether you are using `stack` or
`cabal`.
-}
module File (withModuleDataFile
            , moduleDataFile
            , dataFileName
            , baseDir
            , dataFilePath
            ) where

import Control.Exception (IOException, catch)
import Data.List (find)
import Language.Haskell.TH (Q, location, Loc(loc_filename), runIO
                           , reportWarning)
import Language.Haskell.TH.Syntax (Module(..), PkgName(..), ModName(..))
import System.Directory (getCurrentDirectory, canonicalizePath
                        , getDirectoryContents, doesDirectoryExist
                        , createDirectoryIfMissing)
import System.IO (Handle, IOMode(..), openFile, hClose)
import System.FilePath (takeExtension, takeDirectory, (</>), (<.>))


-- | Run a quote action with the given module data file
withModuleDataFile :: Module -> String -> IOMode -> (Handle -> Q a) -> Q a
withModuleDataFile mod namespace mode fun = do
  h <- moduleDataFile mod namespace mode
  res <- fun h
  runIO $ hClose h
  return res

-- | get a handle to a data file corrisponding to the given
--   module. The second parameter allows you to specify a
--   namespace for data file so that data can be kept private
--   within a particular quasi quoter or family of template
--   haskell functions.
moduleDataFile :: Module -- ^ the module to get the file for
               -> String -- ^ the template haskell namespace
               -> IOMode -- ^ the file mode to open the file with
               -> Q Handle
moduleDataFile mod namespace mode = do
  base <- baseDir
  dataFile <- dataFileName mod namespace
  runIO $ createDirectoryIfMissing True base
  runIO $ openFile (base </> dataFile) mode

-- | get just the name of the file
dataFileName :: Module -> String -> Q FilePath
dataFileName (Module (PkgName pkg) (ModName mod)) namespace = do
  let dash s1 s2 = s1 ++ ('-' : s2)
      legal = all (not . (`elem`"/\\\0\n\t "))
  if (legal pkg && legal mod && legal namespace)
    then return $ pkg `dash` mod `dash` namespace <.> "dat"
  else fail $ "th-module-data:dataFileName illegal pkg, mod, or namespace"
              ++ " pkg=" ++ show pkg
              ++ " mod=" ++ show mod
              ++ " namespace=" ++ show namespace

-- | generate the data file path from a module name and a namespace
dataFilePath :: Module -> String -> Q FilePath
dataFilePath m n = do
  base <- baseDir
  file <- dataFileName m n
  return $ base </> file

-- | calculate the base directory for the `th-module-data` output
baseDir :: Q FilePath
baseDir = do
  rootDir <- pathRelativeToCabalPackage "."
  let stackDir = rootDir </> ".stack-work"
      distDir = rootDir </> "dist"
  stackExists <- runIO $ doesDirectoryExist stackDir
  if stackExists
    then return $ stackDir </> "th-module-data"
  else do
    distExists <- runIO $ doesDirectoryExist distDir
    if distExists
      then return $ distDir </> "th-module-data"
    else do
      reportWarning "Failed to find either `dist` or `.stack-work` directories. Caching compile data in the root directory"
      return $ rootDir </> "th-module-data"



----------------------------------------------------------------------------
--                                                                        --
--  The below code is stolen from `th-utilities`, which is MIT licenced.  --
--  The source can be found at: https://github.com/fpco/th-utilities.     --
--                                                                        --
----------------------------------------------------------------------------


-- Note that this utility does _not_ invoke 'qAddDependentFile'.
pathRelativeToCabalPackage :: FilePath -> Q FilePath
pathRelativeToCabalPackage fp = do
    loc <- location
    parent <-
        if loc_filename loc == "<interactive>"
            then runIO getCurrentDirectory
            else do
                mcanonical <- runIO $ fmap Just (canonicalizePath (loc_filename loc))
                   `catch` \(_err :: IOException) -> return Nothing
                mcabalFile <- runIO $ maybe (return Nothing) findCabalFile mcanonical
                case mcabalFile of
                    Just cabalFile -> return (takeDirectory cabalFile)
                    Nothing -> do
                        reportWarning "Failed to find cabal file, in order to resolve relative paths in TH.  Using current working directory instead."
                        runIO getCurrentDirectory
    return (parent </> fp)

-- | Given the path to a file or directory, search parent directories
-- for a .cabal file.
findCabalFile :: FilePath -> IO (Maybe FilePath)
findCabalFile dir = do
    let parent = takeDirectory dir
    contents <- getDirectoryContents parent
    case find (\fp -> takeExtension fp == ".cabal") contents of
        Nothing
            | parent == dir -> return Nothing
            | otherwise -> findCabalFile parent
        Just fp -> return (Just (parent </> fp))
