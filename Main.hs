module Main where

import System.Directory

main :: IO ()
main = do d <- getCurrentDirectory
          c <- getDirectoryContents $ d++"/cnc"
          print c

