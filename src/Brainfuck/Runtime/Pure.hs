{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module Brainfuck.Runtime.Pure (runPureRuntime) where

import Brainfuck.Runtime
import qualified Brainfuck.Runtime.STTape as Tape
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWS
import Data.List

newtype PureRuntime s a = PureRuntime
  { runRuntime :: RWST (Tape.STTape s) [CellValue] [CellValue] (MaybeT (ST s)) a
  }
  deriving (Functor)

instance Applicative (PureRuntime s) where
  pure = PureRuntime . pure
  (<*>) = ap

instance Monad (PureRuntime s) where
  a >>= f = PureRuntime $ runRuntime a >>= (runRuntime . f)

liftTape :: (Tape.STTape s -> ST s a) -> PureRuntime s a
liftTape = PureRuntime . (ask >>=) . ((lift . lift) .)

instance RuntimeMonad (PureRuntime s) where
  input = PureRuntime $ do
    (x : rest) <- get
    put rest
    return x
  output = PureRuntime . tell . singleton
  shift = liftTape . Tape.shift
  readAt = liftTape . Tape.read
  modifyAt = curry $ liftTape . uncurry Tape.modify

runPureRuntime :: (forall s. PureRuntime s ()) -> [CellValue] -> Maybe [CellValue]
runPureRuntime runtime input = fmap snd $ runST $ do
  tape <- Tape.newZeroed
  runMaybeT $ evalRWST (runRuntime runtime) tape input
