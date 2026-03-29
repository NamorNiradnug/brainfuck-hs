{-# LANGUAGE FlexibleContexts #-}

module Brainfuck.Parser
  ( Command (..),
    Program,
    parser,
    Brainfuck.Parser.parse,
  )
where

import Data.Functor
import Data.Functor.Identity
import Text.Parsec

data Command
  = Loop [Command]
  | ShiftLeft
  | ShiftRight
  | Increment
  | Decrement
  | Input
  | Output
  deriving (Eq, Show)

type Program = [Command]

parser :: (Stream s m Char) => ParsecT s u m Program
parser = parserImpl <* eof
  where
    parserImpl = spaces >> (shiftLeft <|> shiftRight <|> increment <|> decrement <|> input <|> output <|> loop) `endBy` spaces
    shiftLeft = char '<' $> ShiftLeft
    shiftRight = char '>' $> ShiftRight
    increment = char '+' $> Increment
    decrement = char '-' $> Decrement
    input = char ',' $> Input
    output = char '.' $> Output
    loop = between (char '[') (char ']') parserImpl <&> Loop

parse :: (Stream s Identity Char) => SourceName -> s -> Either ParseError Program
parse = Text.Parsec.parse parser
