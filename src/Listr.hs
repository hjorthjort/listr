module Listr
    ( listContents
    ) where

import System.IO (stderr, hPutStrLn)
import System.Directory (
    getDirectoryContents
  , doesFileExist
  , doesPathExist
  , doesDirectoryExist
  , pathIsSymbolicLink
  , canonicalizePath
  , getCurrentDirectory
  , getSymbolicLinkTarget
  )
import System.FilePath.Posix ((</>), makeRelative, takeDirectory)

-- | Recursively get all non-hidden (starting with ".") files from a given directory.
-- Follows symbolic links.
listContents :: FilePath -> IO [String]
listContents path = do path' <- filterPaths doesPathExist [path]
                       case path' of
                         [] -> warn path >> return []
                         [p] -> do isFile <- doesFileExist p
                                   if isFile
                                     then return ["." </> p]
                                     else listContents' p
  where warn path = hPutStrLn stderr $ "Warning: " ++ path ++ " is not a valid path"

listContents' :: FilePath -> IO [FilePath]
listContents' path = do contents <- getDirectoryContents path
                        currPath <- getCurrentDirectory
                        -- A list of conditions the contents of the path must satisfy.
                        let conds = [
                              (/= '.').head -- Ignore hidden (Linux only).
                              ] :: [FilePath -> Bool]
                            filtered = filter (\x -> all ($x) conds) contents
                            paths = map ((path </>)) filtered
                        files <- onlyFiles paths
                        dirs  <- onlyDirs paths
                        recursiveFiles <- concat <$> mapM listContents dirs
                        let uncleanRes = files ++ recursiveFiles
                            cleanedRes = makeRelative currPath <$> uncleanRes
                        return cleanedRes
  where
    onlyFiles :: [FilePath] -> IO [FilePath]
    onlyFiles = filterPaths doesFileExist
    onlyDirs :: [FilePath] -> IO [FilePath]
    onlyDirs = filterPaths doesDirectoryExist

-- | Retain only paths matching the given (monadic) predicate.
-- Follows symbolic links and replace paths with what they point to.
-- Drop paths that do not point anywhere.
filterPaths :: (FilePath -> IO Bool) -> [FilePath] -> IO [FilePath]
filterPaths _     []     = return []
filterPaths predM (f:fs) =
  do exist <- doesPathExist f
     rest <- filterPaths predM fs
     if not exist
       then return rest
       else do sym <- pathIsSymbolicLink f
               if sym
                 then do targ <- getSymbolicLinkTarget f
                         let dir = takeDirectory f
                             f'  = dir </> targ
                         exist <- doesPathExist f'
                         if exist
                           then filterPaths predM (f':fs)
                           else return rest
                 else do valid <- predM f
                         return $ if valid then f:rest else rest
