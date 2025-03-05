module Project
  ( ProjectTarget (..)
  , extractTargets
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
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

linkCommandNames :: Set Text
linkCommandNames = Set.fromList
  [ "target_link_libraries"
  ]

linkTypeSpecifiers :: Set Text
linkTypeSpecifiers = Set.fromList
  [ "PRIVATE"
  , "PUBLIC"
  , "INTERFACE"
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

extractTargetDefinitions :: CMakeFile -> [ProjectTarget]
extractTargetDefinitions rawFile =
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

extractLinkCommands :: CMakeFile -> Map Text [Text]
extractLinkCommands rawFile =
  let
    commands = stripCommandsFromFile rawFile
    linkCommands = filter ((`Set.member` linkCommandNames) . commandIdent) commands
  in
    Map.fromListWith
      (++)
      $ fmap makeDeps linkCommands
  where
    makeDeps (CMakeStrippedCommandInvocation  _ args) =
      (argumentValue $ head args
      , filter (not . (`Set.member` linkTypeSpecifiers)) $ fmap argumentValue $ tail args)

extractTargets :: CMakeFile -> [ProjectTarget]
extractTargets rawFile =
  let
    definitions = extractTargetDefinitions rawFile
  in
    fmap addDeps definitions
  where
    addDeps target =
      target {
        dependencies =
          dependencies target
          ++ Map.findWithDefault [] (targetName target) links
      }
    links = extractLinkCommands rawFile
