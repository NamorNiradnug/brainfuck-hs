module Brainfuck.Interpreter (interpret) where

import Brainfuck.Parser
import Brainfuck.Runtime
import Prelude hiding (read)

interpretCommand :: (RuntimeMonad m) => Command -> m ()
interpretCommand = \case
  Loop body -> loop (interpret body)
  ShiftLeft -> shiftLeft
  ShiftRight -> shiftRight
  Increment -> modify (+ 1)
  Decrement -> modify (subtract 1)
  Input -> inputToTape
  Output -> outputFromTape

interpret :: (RuntimeMonad m) => Program -> m ()
interpret = mapM_ interpretCommand
