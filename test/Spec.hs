module Spec where
import Debug.Trace (traceShow)
import Test.QuickCheck
    ( elements,
      listOf,
      quickCheck,
      verboseCheck,
      Arbitrary(arbitrary),
      Gen,
      Positive(Positive),
      PrintableString(getPrintableString) )

import LibLispVal

--https://stackoverflow.com/questions/35726256/quickcheck-on-custom-datatype



prop_showVal_numbers :: LispVal -> Bool
prop_showVal_numbers (Number x)
 = showVal (Number x) == show x


prop_showError_notFunction :: LispError -> Bool
prop_showError_notFunction (NotFunction err message)
 = showError (NotFunction err message) == err ++ ": \"" ++ message ++ "\""


instance Arbitrary LispVal where
    arbitrary = do
        Positive num <- arbitrary
        return $ Number num


arbitraryPrintableString :: Gen String
arbitraryPrintableString = getPrintableString <$> arbitrary

instance Arbitrary LispError where
    arbitrary = do
        err <- arbitraryPrintableString
        return $ NotFunction (err ++ "testerror") "function123" 


main :: IO ()
main = do
    quickCheck prop_showVal_numbers
    quickCheck prop_showError_notFunction
    putStrLn "Done"


