module DirectoryTraverse
  ( traverseDirectoryTree
  )
  where

import System.Directory
import System.FilePath

traverseDirectoryTree :: Monoid a => (FilePath -> [FilePath] -> IO a) -> FilePath -> IO a
traverseDirectoryTree visit root = do
  curContents <- listDirectory root
  curResult <- visit root curContents
  res <- mapM (\c -> do
    let fullPath = root </> c
    isDir <- doesDirectoryExist fullPath
    if isDir
      then (traverseDirectoryTree visit fullPath)
      else return mempty
    ) curContents
  return $ curResult <> mconcat res
