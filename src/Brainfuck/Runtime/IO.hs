module Brainfuck.Runtime.IO (runIORuntime) where

import Brainfuck.Runtime
import qualified Brainfuck.Runtime.STTape as Tape
import Control.Monad
import Control.Monad.Reader
import Control.Monad.ST
import Data.Char

newtype IORuntime a = IORuntime {runRuntime :: ReaderT (Tape.STTape RealWorld) IO a}

instance Functor IORuntime where
  fmap f = IORuntime . fmap f . runRuntime

instance Applicative IORuntime where
  pure = IORuntime . pure
  (<*>) = ap

instance Monad IORuntime where
  a >>= f = IORuntime $ runRuntime a >>= (runRuntime . f)

liftTape :: (Tape.STTape RealWorld -> ST RealWorld a) -> IORuntime a
liftTape = IORuntime . (ask >>=) . ((lift . stToIO) .)

instance RuntimeMonad IORuntime where
  input = IORuntime $ lift $ ord <$> getChar
  output = IORuntime . lift . putChar . chr
  shift = liftTape . Tape.shift
  readAt = liftTape . Tape.read
  modifyAt = curry $ liftTape . uncurry Tape.modify

runIORuntime :: IORuntime a -> IO a
runIORuntime = (stToIO Tape.newZeroed >>=) . runReaderT . runRuntime
