{-# LANGUAGE BlockArguments #-}

module Brainfuck.Runtime.STTape where

import Brainfuck.Runtime
import Brainfuck.Utils.Offset
import Control.Monad.ST
import Data.Array.ST
import Data.STRef

data STTape s = STTape {offsetRef :: STRef s Offset, cellsRef :: STRef s (STUArray s Offset CellValue)}

newZeroed :: ST s (STTape s)
newZeroed = STTape <$> newSTRef offsetZero <*> (newArray (offsetLeft, offsetRight) 0 >>= newSTRef)

shift :: Offset -> STTape s -> ST s ()
shift offset STTape {..} = modifySTRef offsetRef (|+| offset)

read :: Offset -> STTape s -> ST s CellValue
read offset STTape {..} = do
  index <- (offset |+|) <$> readSTRef offsetRef
  cells <- readSTRef cellsRef
  indexRange <- getBounds cells
  if inRange indexRange index
    then readArray cells index
    else return 0

modify :: Offset -> (CellValue -> CellValue) -> STTape s -> ST s ()
modify offset f STTape {..} = do
  index <- (offset |+|) <$> readSTRef offsetRef
  cells <- readSTRef cellsRef
  indexRange@(lower, upper) <- getBounds cells
  if inRange indexRange index
    then modifyArray cells index f
    else do
      let newBounds = if index < lower then (2 |*| index, upper) else (lower, 2 |*| index)
      newCells <- newGenArray newBounds $ \i -> if inRange indexRange i then readArray cells i else pure 0
      modifyArray newCells index f
      writeSTRef cellsRef newCells
