module Listr
    ( listContents
    ) where

import System.Directory (getDirectoryContents, doesFileExist, pathIsSymbolicLink, canonicalizePath, getCurrentDirectory)
import System.FilePath.Posix ((</>), makeRelative)

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
