module Listr
    ( listContents
    ) where

import System.Directory (
    getDirectoryContents
  , doesFileExist
  , doesDirectoryExist
  , pathIsSymbolicLink
  , canonicalizePath
  , getCurrentDirectory
  , getSymbolicLinkTarget
  )
import System.FilePath.Posix ((</>), makeRelative, takeDirectory)

listContents :: FilePath -> IO [String]
listContents path = do contents <- getDirectoryContents path
                       currPath <- getCurrentDirectory
                       -- A list of conditions the paths must satisfy.
                       let conds = [
                             (/= '.').head -- Ignore hidden (Linux only).
                             ] :: [FilePath -> Bool]
                           filtered = filter (\x -> all ($x) conds) contents
                           paths = map (makeRelative currPath . (path </>)) filtered
                       onlyFiles paths
  where onlyFiles []    = return []
        onlyFiles (f:fs) = do exist <- doesFileExist f
                              sym   <- pathIsSymbolicLink f
                              rest  <- onlyFiles fs
                              return $ if exist || sym then f:rest else rest
    onlyFiles :: [FilePath] -> IO [FilePath]
    onlyFiles = filterPaths doesFileExist
-- | Retain only paths matching the given (monadic) predicate.
-- Follows symbolic links.
filterPaths :: (FilePath -> IO Bool) -> [FilePath] -> IO [FilePath]
filterPaths _     []     = return []
filterPaths predM (f:fs) = do sym <- pathIsSymbolicLink f
                              if sym
                                then do f' <- getSymbolicLinkTarget f
                                        let dir = takeDirectory f
                                        filterPaths predM ((dir</>f'):fs)
                                else do valid <- predM f
                                        rest  <- filterPaths predM fs
                                        return $ if valid then f:rest else rest
