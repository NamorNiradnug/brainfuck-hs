{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Brainfuck.Compiler
  ( Sequence (..),
    Update (..),
    FusedCommand (..),
    ExtendedCommand (..),
    CompiledProgram,
    compile,
  )
where

import Brainfuck.Parser (Command, Program)
import qualified Brainfuck.Parser as Parser
import Brainfuck.Runtime
import Brainfuck.Utils.Offset
import Data.List hiding (singleton)
import Data.Maybe

data Sequence a = Empty | !a :> !(Sequence a) deriving (Functor, Foldable)

data Update = Update {offset :: !Offset, change :: !CellValue}

update :: Sequence Update -> Update -> Sequence Update
update Empty update = update :> Empty
update (head@(Update off x) :> tail) upd@(Update off' x') =
  if off == off'
    then if x + x' /= 0 then Update off (x + x') :> tail else tail
    else head :> update tail upd

data FusedCommand = FusedCommand {updates :: !(Sequence Update), shift :: !Offset}

noop :: FusedCommand
noop = FusedCommand Empty offsetZero

updateCommand :: FusedCommand -> Update -> FusedCommand
updateCommand FusedCommand {..} upd = FusedCommand {updates = update updates upd, ..}

shiftCommand :: FusedCommand -> Offset -> FusedCommand
shiftCommand FusedCommand {..} offset = FusedCommand {shift = shift |+| offset, ..}

data ExtendedCommand
  = Loop [ExtendedCommand]
  | Fused FusedCommand
  | Input
  | Output

type CompiledProgram = [ExtendedCommand]

fuse :: ExtendedCommand -> Parser.Command -> Maybe ExtendedCommand
fuse (Fused fused@FusedCommand {..}) = \case
  Parser.Increment -> Just $ Fused $ updateCommand fused $ Update shift 1
  Parser.Decrement -> Just $ Fused $ updateCommand fused $ Update shift (-1)
  Parser.ShiftLeft -> Just $ Fused $ shiftCommand fused offsetLeft
  Parser.ShiftRight -> Just $ Fused $ shiftCommand fused offsetRight
  _ -> Nothing
fuse _ = const Nothing

compileCommand :: Command -> ExtendedCommand
compileCommand = \case
  Parser.Loop body -> Loop (compile body)
  Parser.Increment -> Fused $ updateCommand noop $ Update offsetZero 1
  Parser.Decrement -> Fused $ updateCommand noop $ Update offsetZero (-1)
  Parser.ShiftLeft -> Fused $ shiftCommand noop offsetLeft
  Parser.ShiftRight -> Fused $ shiftCommand noop offsetRight
  Parser.Input -> Input
  Parser.Output -> Output

compile :: Program -> CompiledProgram
compile = reverse . foldl' appendCommand []
  where
    appendCommand :: CompiledProgram -> Command -> CompiledProgram
    appendCommand accum command = fromMaybe (compileCommand command : accum) $ do
      (last, prefix) <- uncons accum
      fused <- fuse last command
      return (fused : prefix)
