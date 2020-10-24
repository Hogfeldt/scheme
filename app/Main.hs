module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

import LibParser
import LibLispVal
import LibEval

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

main :: IO ()
main = getArgs >>= print . eval . readExpr . head 
