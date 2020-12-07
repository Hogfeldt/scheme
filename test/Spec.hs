


module Spec where

import Test.HUnit
import Test.QuickCheck

import LibLispVal




merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)
  | x < y     = x : merge xs ys
  | otherwise = y : merge xs ys
merge _      _      = []


prop_numElements :: [Integer] -> [Integer] -> Bool
prop_numElements xs ys
  = length xs + length ys == length (merge xs ys)


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
    quickCheck prop_showVal_strings
    putStrLn "Done"


