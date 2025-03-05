module Main where

import qualified Data.Text.IO as TextIO
import System.Environment
import Text.Parsec

import CMakeParse
import CMakeUtils

main :: IO ()
main = do
  args <- getArgs
  file <- TextIO.readFile (args !! 0)
  let m_cmake = parse parseCMake (args !! 0) file
  case m_cmake of
    Left e -> print e
    Right cmake -> do
      print cmake
      print $ stripCommandsFromFile cmake
  return ()
