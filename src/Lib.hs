module Lib
    ( listContents
    ) where

import System.Directory (getDirectoryContents)

listContents :: FilePath -> IO [String]
listContents path = do conts <- getDirectoryContents path
                       let conds  = [not.null, (/= '.').head]
                       let filt   = \x -> all ($x) conds
                       let conts' = filter filt conts
                       return conts'
