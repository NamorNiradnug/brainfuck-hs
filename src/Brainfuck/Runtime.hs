module Brainfuck.Runtime
  ( CellValue,
    Offset (..),
    negate,
    RuntimeMonad (..),
    loop,
  )
where

import Control.Monad
import Prelude hiding (negate, read)
import qualified Prelude

type CellValue = Int

newtype Offset = MkOffset {getOffset :: Int}

negate :: Offset -> Offset
negate = MkOffset . Prelude.negate . getOffset

class (Monad m) => RuntimeMonad m where
  {-# MINIMAL input, output, (shift | shiftLeft, shiftRight), (read | readAt), (modify | modifyAt) #-}
  shift :: Offset -> m ()
  shift (MkOffset offset) = case compare offset 0 of
    LT -> replicateM_ (-offset) shiftLeft
    EQ -> return ()
    GT -> replicateM_ offset shiftRight

  shiftLeft :: m ()
  shiftLeft = shift (MkOffset (-1))

  shiftRight :: m ()
  shiftRight = shift (MkOffset 1)

  readAt :: Offset -> m CellValue
  readAt offset = shift offset *> read <* shift (negate offset)

  read :: m CellValue
  read = readAt (MkOffset 0)

  modifyAt :: Offset -> (CellValue -> CellValue) -> m ()
  modifyAt offset f = shift offset >> modify f >> shift (negate offset)

  modify :: (CellValue -> CellValue) -> m ()
  modify = modifyAt (MkOffset 0)

  input :: m CellValue
  output :: CellValue -> m ()

loop :: (RuntimeMonad m) => m () -> m ()
loop body = runLoop where runLoop = read >>= flip unless (body >> runLoop) . (== 0)
