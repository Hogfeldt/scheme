


module Spec where

import Test.HUnit
import LibLispVal

test1 :: Test
test1 = TestCase (assertEqual "1 = 1" 69 420)


test2 :: Test
test2 = TestCase (assertEqual "1 = 1" (testFun 2) 1)


tests :: Test
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

main :: IO Counts
main = runTestTT tests
