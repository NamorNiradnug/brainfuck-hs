module Brainfuck.Utils.Offset
  ( Offset (..),
    neg,
    (|+|),
    (|*|),
    offsetZero,
    offsetLeft,
    offsetRight,
  )
where

import Data.Function
import Data.Ix

newtype Offset = MkOffset {getOffset :: Int}
  deriving (Show, Eq, Ord, Ix)

neg :: Offset -> Offset
neg = MkOffset . negate . getOffset

(|+|) :: Offset -> Offset -> Offset
x |+| y = MkOffset $ on (+) getOffset x y

(|*|) :: Int -> Offset -> Offset
x |*| y = MkOffset $ x * getOffset y

offsetZero :: Offset
offsetZero = MkOffset 0

offsetLeft :: Offset
offsetLeft = MkOffset (-1)

offsetRight :: Offset
offsetRight = MkOffset 1
