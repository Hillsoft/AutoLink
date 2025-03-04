module Main where

import qualified Data.Text.IO as TextIO
import System.Environment
import Text.Parsec

import CMakeParse

main :: IO ()
main = do
  args <- getArgs
  file <- TextIO.readFile (args !! 0)
  print $ parse parseCMake (args !! 0) file
  return ()
