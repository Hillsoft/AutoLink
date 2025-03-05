module CMakeParse
  ( parseCMake
  , CMakeFile (..)
  , CMakeFileElement (..)
  , CMakeLineEnding (..)
  , CMakeCommandInvocation (..)
  , CMakeIdentifier (..)
  , CMakeArguments (..)
  , CMakeSeperatedArguments (..)
  , CMakeSeparation (..)
  , CMakeArgument (..)
  , CMakeBracketComment (..)
  , CMakeLineComment (..)
  ) where

import Control.Monad
import Data.Char (isAlphaNum, isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parsec

newtype CMakeFile = CMakeFile [CMakeFileElement]
  deriving Show

cmakeFile :: Parsec Text s CMakeFile
cmakeFile = CMakeFile <$> many cmakeFileElement

data CMakeFileElement =
  FileElementCommandInvocation CMakeCommandInvocation CMakeLineEnding
  | FileElementBracketComment [CMakeBracketComment] CMakeLineEnding
  deriving Show

cmakeFileElement :: Parsec Text s CMakeFileElement
cmakeFileElement =
  (FileElementBracketComment <$> many (char '(' *> char ')' *> return CMakeBracketComment) <*> cmakeLineEnding)
  <|> (FileElementCommandInvocation <$>
    cmakeCommandInvocation <*> cmakeLineEnding)

data CMakeLineEnding = CMakeLineEnding (Maybe CMakeLineComment)
  deriving Show

cmakeLineEnding :: Parsec Text s CMakeLineEnding
cmakeLineEnding =
  CMakeLineEnding <$> (
    (Just <$> cmakeLineComment)
    <|> (newline *> return Nothing)
  )

data CMakeCommandInvocation = CMakeCommandInvocation CMakeIdentifier CMakeArguments
  deriving Show

cmakeCommandInvocation :: Parsec Text s CMakeCommandInvocation
cmakeCommandInvocation = flip label "command" $ do
  _ <- many cmakeSpace
  commandName <- cmakeIdentifier
  _ <- many cmakeSpace
  _ <- char '('
  args <- cmakeArguments
  _ <- char ')'
  return $ CMakeCommandInvocation commandName args

newtype CMakeIdentifier = CMakeIdentifier Text
  deriving Show

cmakeIdentifier :: Parsec Text s CMakeIdentifier
cmakeIdentifier = flip label "identifier" $ do
  firstChar <- letter <|> char '_'
  remaining <- many (alphaNum <|> char '_')
  return $ CMakeIdentifier $ Text.pack (firstChar:remaining)

data CMakeArguments = CMakeArguments (Maybe CMakeArgument) [CMakeSeperatedArguments]
  deriving Show

cmakeArguments :: Parsec Text s CMakeArguments
cmakeArguments = do
  arg <- (Just <$> cmakeArgument <|> return Nothing)
  sepArgs <- many cmakeSeparatedArguments
  return $ CMakeArguments arg sepArgs

data CMakeSeperatedArguments =
  SeperatedArgumentsA [CMakeSeparation] (Maybe CMakeArgument)
  | SeperatedArgumentsB [CMakeSeparation] CMakeArguments
  deriving Show

cmakeSeparatedArguments :: Parsec Text s CMakeSeperatedArguments
cmakeSeparatedArguments = do
  seps <- many cmakeSeparation
  (
    SeperatedArgumentsB seps <$>
    (char '(' *> cmakeArguments <* char ')')
    ) <|> (
      guard (not $ null seps) >>
      SeperatedArgumentsA seps <$> (Just <$> cmakeArgument <|> return Nothing)
    )

data CMakeSeparation = CMakeSeparation (Maybe CMakeLineEnding)
  deriving Show

cmakeSeparation :: Parsec Text s CMakeSeparation
cmakeSeparation = CMakeSeparation <$> ((cmakeSpace *> return Nothing) <|> (Just <$> cmakeLineEnding))

data CMakeArgument = CMakeArgument Text
  deriving Show

cmakeArgument :: Parsec Text s CMakeArgument
cmakeArgument = flip label "argument" $ cmakeBracketArgument <|> cmakeQuotedArgument <|> cmakeUnquotedArgument

cmakeBracketArgument :: Parsec Text s CMakeArgument
cmakeBracketArgument = do
  _ <- char '['
  initEqCount <- length <$> many (char '=')
  _ <- char '['
  argContents <- Text.pack <$> go initEqCount
  return $ CMakeArgument argContents
  where
    go initEqCount = do
      s <- many (noneOf [']'])
      _ <- char ']'
      curEqCount <- length <$> many (char '=')
      if curEqCount /= initEqCount
        then do
          remainder <- go initEqCount
          return $ s ++ "]" ++ (take curEqCount $ repeat '=') ++ remainder
        else do
          nextChar <- anyChar
          if nextChar == ']'
            then return s
            else do
              remainder <- go initEqCount
              return $ s ++ "]" ++ (take curEqCount $ repeat '=') ++ [nextChar] ++ remainder

cmakeQuotedArgument :: Parsec Text s CMakeArgument
cmakeQuotedArgument = do
  _ <- char '"'
  argContents <- Text.pack <$> many element
  _ <- char '"'
  return $ CMakeArgument argContents
  where
    element =
      noneOf ['\\', '"'] <|> escapeSequence

cmakeUnquotedArgument :: Parsec Text s CMakeArgument
cmakeUnquotedArgument = (CMakeArgument . Text.pack) <$> many1 element
  where
    element = noneOf [' ', '\n', '\t', '\r', '(', ')', '#', '"', '\\', '\''] <|> escapeSequence

escapeSequence :: Parsec Text s Char
escapeSequence = do
  _ <- char '\\'
  identity <|> encoded
  where
    identity = satisfy (not . isAlphaNum)
    encoded =
      (char 't' *> return '\t')
      <|> (char 'r' *> return '\r')
      <|> (char 'n' *> return '\n')

data CMakeBracketComment = CMakeBracketComment
  deriving Show

newtype CMakeLineComment = CMakeLineComment Text
  deriving Show

cmakeLineComment :: Parsec Text s CMakeLineComment
cmakeLineComment = do
  _ <- char '#'
  firstChar <- noneOf ['(']
  remaining <- many $ noneOf ['\r', '\n']
  return $ CMakeLineComment $ Text.pack $ firstChar : remaining

cmakeSpace :: Parsec Text s Char
cmakeSpace = satisfy (\c -> isSpace c && c /= '\r' && c /= '\n')

parseCMake :: Parsec Text s CMakeFile
parseCMake = cmakeFile <* eof
