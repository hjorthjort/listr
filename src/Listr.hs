module Listr
    ( listContents
    ) where

import System.Directory (getDirectoryContents, doesFileExist, pathIsSymbolicLink, canonicalizePath, getCurrentDirectory)
import System.FilePath.Posix ((</>))

listContents :: FilePath -> IO [String]
listContents path = do conts <- getDirectoryContents path
                       currPath <- getCurrentDirectory
                       let conds  = [(/= '.').head]
                       let filt   = \x -> all ($x) conds
                       let nonDots = filter filt conts
                       paths <- mapM (canonicalizePath.((currPath </> path) </>)) nonDots
                       onlyFiles paths
  where onlyFiles []    = return []
        onlyFiles (f:fs) = do exist <- doesFileExist f
                              sym   <- pathIsSymbolicLink f
                              rest  <- onlyFiles fs
                              return $ if exist || sym then f:rest else rest
