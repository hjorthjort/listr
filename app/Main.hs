module Main where

import Lib
import System.Environment

main :: IO ()
main = do args <- getArgs
          let args' = if null args then ["."] else args
          allFiles <- mapM listContents args'
          putStr $ unlines $ concat allFiles
