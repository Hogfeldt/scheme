


module Spec where

import Test.HUnit
import Test.QuickCheck

import LibLispVal



testShowVal :: Test
testShowVal = TestCase (assertEqual "showVal" "1" (showVal (Number 1)))


testUnwordsList :: Test
testUnwordsList = TestCase (assertEqual "unwordsList" "1 2 3" (unwordsList [Number 1, Number 2, Number 3]))


testShowError :: Test
testShowError = TestCase (
    assertEqual "showError" "Unknown error" (showError (Default "This string is never used"))
    )


tests :: Test
tests = TestList [
    TestLabel "testShowVal" testShowVal,
    TestLabel "testUnwordsList" testUnwordsList,
    TestLabel "testShowError" testShowError
    ]

main :: IO Counts
main = do
    runTestTT tests
