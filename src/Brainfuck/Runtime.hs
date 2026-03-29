module Brainfuck.Runtime
  ( CellValue,
    RuntimeMonad (..),
    loop,
    inputToTape,
    outputFromTape,
  )
where

import Brainfuck.Utils.Offset
import Control.Monad
import Prelude hiding (read)

type CellValue = Int

class (Monad m) => RuntimeMonad m where
  {-# MINIMAL input, output, (shift | shiftLeft, shiftRight), (read | readAt), (modify | modifyAt) #-}
  shift :: Offset -> m ()
  shift (MkOffset offset) = case compare offset 0 of
    LT -> replicateM_ (-offset) shiftLeft
    EQ -> return ()
    GT -> replicateM_ offset shiftRight

  shiftLeft :: m ()
  shiftLeft = shift offsetLeft

  shiftRight :: m ()
  shiftRight = shift offsetRight

  readAt :: Offset -> m CellValue
  readAt offset = shift offset *> read <* shift (neg offset)

  read :: m CellValue
  read = readAt offsetZero

  modifyAt :: Offset -> (CellValue -> CellValue) -> m ()
  modifyAt offset f = shift offset >> modify f >> shift (neg offset)

  modify :: (CellValue -> CellValue) -> m ()
  modify = modifyAt offsetZero

  input :: m CellValue
  output :: CellValue -> m ()

loop :: (RuntimeMonad m) => m () -> m ()
loop body = runLoop where runLoop = read >>= flip unless (body >> runLoop) . (== 0)

inputToTape :: (RuntimeMonad m) => m ()
inputToTape = input >>= modify . const

outputFromTape :: (RuntimeMonad m) => m ()
outputFromTape = read >>= output
