module Brainfuck.Executor (execute) where

import Brainfuck.Compiler
import Brainfuck.Runtime
import Prelude hiding (read)

executeUpdate :: (RuntimeMonad m) => Update -> m ()
executeUpdate Update {..} = modifyAt offset (+ change)

executeUpdates :: (RuntimeMonad m, Foldable f) => f Update -> m ()
executeUpdates = mapM_ executeUpdate

repeatUpdate :: Int -> Update -> Update
repeatUpdate times update@(Update {..}) = update {change = times * change}

executeSimpleLoop :: (RuntimeMonad m) => CellValue -> CellValue -> Sequence Update -> m ()
executeSimpleLoop inductionVariable inductionStep updates
  | inductionVariable == 0 = pure ()
  | inductionStep /= 0,
    steps > 0,
    reminder == 0 =
      executeUpdates (fmap (repeatUpdate steps) updates) >> modify (const 0)
  -- TODO proper exception handling in RuntimeMonad
  | otherwise = error $ "Infinite loop detected " ++ show inductionStep ++ " " ++ show inductionVariable
  where
    (steps, reminder) = inductionVariable `divMod` inductionStep

executeCommand :: (RuntimeMonad m) => ExtendedCommand -> m ()
executeCommand = \case
  Loop body -> loop (execute body)
  Fused FusedCommand {..} ->
    executeUpdates updates >> Brainfuck.Runtime.shift shift
  SimpleLoop {..} -> do
    inductionVariable <- read
    executeSimpleLoop inductionVariable inductionStep iteration
  Input -> inputToTape
  Output -> outputFromTape

execute :: (RuntimeMonad m) => CompiledProgram -> m ()
execute = mapM_ executeCommand
