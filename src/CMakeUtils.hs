module CMakeUtils
  ( CMakeStrippedCommandInvocation (..)
  , stripCommandsFromFile
  ) where

import CMakeParse

data CMakeStrippedCommandInvocation = CMakeStrippedCommandInvocation CMakeIdentifier [CMakeArgument]
  deriving Show

stripInvocation :: CMakeCommandInvocation -> CMakeStrippedCommandInvocation
stripInvocation (CMakeCommandInvocation ident rootArgs) =
  CMakeStrippedCommandInvocation ident (stripArgs rootArgs)
  where
    stripArgs (CMakeArguments (Just a) sepArgs) = a : concatMap stripSepArgs sepArgs
    stripArgs (CMakeArguments Nothing sepArgs) = concatMap stripSepArgs sepArgs
    stripSepArgs (SeperatedArgumentsA _ (Just a)) = [a]
    stripSepArgs (SeperatedArgumentsA _ Nothing) = []
    stripSepArgs (SeperatedArgumentsB _ args) = stripArgs args

stripCommandsFromFile :: CMakeFile -> [CMakeStrippedCommandInvocation]
stripCommandsFromFile (CMakeFile elems) = concatMap go elems
  where
    go (FileElementCommandInvocation i _) = [stripInvocation i]
    go (FileElementBracketComment _ _) = []
