module Main where

import Control.Monad
import System.Directory
import System.Environment
import System.FilePath

import DirectoryTraverse

main :: IO ()
main = do
  args <- getArgs
  traverseDirectoryTree (\p _ -> do
    isCmakeDir <- doesFileExist (p </> "CMakeLists.txt")
    if (isCmakeDir)
      then putStrLn p *> return [p]
      else return []
    ) (args !! 0)
  return ()
