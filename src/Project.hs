module Project
  ( ProjectTarget (..)
  , extractTargets
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

import CMakeParse
import CMakeUtils

targetDefinitionCommandNames :: Set Text
targetDefinitionCommandNames = Set.fromList
  [ "add_executable"
  , "add_library"
  ]

targetTypeSpecifiers :: Set Text
targetTypeSpecifiers = Set.fromList
  [ "STATIC"
  , "SHARED"
  , "MODULE"
  , "INTERFACE"
  , "EXCLUDE_FROM_ALL"
  ]

commandIdent :: CMakeStrippedCommandInvocation -> Text
commandIdent (CMakeStrippedCommandInvocation (CMakeIdentifier identName) _) = identName

argumentValue :: CMakeArgument -> Text
argumentValue (CMakeArgument val) = val

data ProjectTarget = ProjectTarget {
  targetName :: Text,
  sourceFiles :: [Text],
  dependencies :: [Text]
  }
  deriving Show

extractTargets :: CMakeFile -> [ProjectTarget]
extractTargets rawFile =
  let
    commands = stripCommandsFromFile rawFile
    targetDefinitionCommands = filter ((`Set.member` targetDefinitionCommandNames) . commandIdent) commands
  in
    fmap makeTarget targetDefinitionCommands
  where
    makeTarget (CMakeStrippedCommandInvocation _ args) =
      ProjectTarget
        (argumentValue $ head args)
        (filter (not . (`Set.member` targetTypeSpecifiers)) $ fmap argumentValue $ tail args)
        []
