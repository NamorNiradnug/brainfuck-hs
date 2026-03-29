module Tests.Parser (tests) where

import Brainfuck.Parser
import Data.Either
import Data.List
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Examples

tests :: TestTree
tests = testGroup "Parser" [parseExamples, parseWithSpaces, rejectInvalid]

parseExamples :: TestTree
parseExamples = testGroup "Examples" $ map parseExample allExamples

parseExample :: Example -> TestTree
parseExample Example {..} =
  testCase name $
    assertBool "examples should be parsed" $
      isRight $
        parse name code

parseWithSpaces :: TestTree
parseWithSpaces =
  testCase (name ++ " with spaces") $
    assertEqual "spaces whould be ignored" (parse name code) (parse name (intersperse ' ' code))
  where
    Example name code = helloWorld

shouldReject :: (String, String) -> TestTree
shouldReject (name, code) =
  testCase name $
    assertBool "invalid code should be rejected" $
      isLeft $
        parse name code

rejectInvalid :: TestTree
rejectInvalid =
  testGroup "Invalid" $
    map
      shouldReject
      [("unknown symbols", "++!++"), ("unmatched brackets", "++>>[++,.[]")]
