module Main (main) where

import Test.Tasty
import qualified Tests.Compiler as Compiler
import qualified Tests.Executor as Executor
import qualified Tests.Interpreter as Interpreter
import qualified Tests.Parser as Parser

tests :: TestTree
tests = testGroup "Tests" [Parser.tests, Interpreter.tests, Compiler.tests, Executor.tests]

main :: IO ()
main = defaultMain tests
