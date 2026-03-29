module Brainfuck.Executor (execute) where

import Brainfuck.Compiler
import Brainfuck.Runtime
import Prelude hiding (read)

executeCommand :: (RuntimeMonad m) => ExtendedCommand -> m ()
executeCommand = \case
  Loop body -> loop (execute body)
  Fused FusedCommand {..} ->
    mapM_ (\(Update offset x) -> modifyAt offset (+ x)) updates >> Brainfuck.Runtime.shift shift
  Input -> inputToTape
  Output -> outputFromTape

execute :: (RuntimeMonad m) => CompiledProgram -> m ()
execute = mapM_ executeCommand
