


module Spec where

import Test.HUnit
import Test.QuickCheck

import LibLispVal

--https://stackoverflow.com/questions/35726256/quickcheck-on-custom-datatype



prop_showVal_numbers :: LispVal -> Bool
prop_showVal_numbers (Number x)
 = showVal (Number x) == show x


prop_showVal_strings :: LispVal -> Bool
prop_showVal_strings (String x)
 = showVal (String x) == show x


instance Arbitrary LispVal where
    arbitrary = do
        Positive num <- arbitrary
        return $ Number num


main :: IO ()
main = do
    quickCheck prop_showVal_numbers
    --quickCheck prop_showVal_strings
    putStrLn "Done"


