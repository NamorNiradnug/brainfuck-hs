module Tests.Interpreter where

import qualified Brainfuck as BF
import Brainfuck.Runtime.Pure (runPureRuntime)
import Data.Char
import Data.Either
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Examples

tests :: TestTree
tests = testGroup "Interpreter" [runHelloWorld]

runExampleWith :: Example -> String -> String -> Assertion
runExampleWith Example {..} input expectedOutput =
  map chr <$> runPureRuntime (BF.interpret $ fromRight undefined $ BF.parse name code) (map ord input)
    @?= Just expectedOutput

runHelloWorld :: TestTree
runHelloWorld = testCase (name helloWorld) $ runExampleWith helloWorld "" "Hello World!\n"
