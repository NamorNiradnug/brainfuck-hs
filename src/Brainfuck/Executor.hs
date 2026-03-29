module Brainfuck.Executor where

import Brainfuck.Compiler
import Brainfuck.Runtime

execute :: (RuntimeMonad m) => CompiledProgram -> m ()
execute = error "TODO"
