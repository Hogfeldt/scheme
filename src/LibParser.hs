module LibParser
    ( parseExpr
    , spaces
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad -- for liftM
import Numeric

import LibLispVal

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseNumber :: Parser LispVal
parseNumber     = try parseSciNegFloat 
               <|> try parseSciPosFloat 
               <|> try parseNegativeFloat 
               <|> try parseFloat  
               <|> try parseNegativeInteger 
               <|> parseInteger

parseInteger :: Parser LispVal
parseInteger = liftM (Number . read) $ many1 digit

parseNegativeInteger :: Parser LispVal
parseNegativeInteger = 
            do
                char '-'
                x <- many1 digit
                return $ Number (read ("-" ++ x))

parseFloat :: Parser LispVal 
parseFloat = 
            do 
                x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float (fst . head $ readFloat (x ++ "." ++ y))

parseNegativeFloat :: Parser LispVal
parseNegativeFloat = 
            do
                char '-'
                x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float (fst . head $ readSigned readFloat ("-" ++ x ++ "." ++ y))

parseSciNegFloat :: Parser LispVal 
parseSciNegFloat = 
            do 
                x <- many1 digit
                char '.'
                y <- many1 digit
                char 'e'
                char '-'
                z <- many1 digit
                return $ Float (fst . head $ readFloat (x ++ "." ++ y ++ "e-" ++ z))

parseSciPosFloat :: Parser LispVal 
parseSciPosFloat = 
            do 
                x <- many1 digit
                char '.'
                y <- many1 digit
                char 'e'
                z <- many1 digit
                return $ Float (fst . head $ readFloat (x ++ "." ++ y ++ "e" ++ z))

parseString :: Parser LispVal 
parseString = 
            do 
                char '"'
                x <- many $ noneOf "\""
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = 
            do
                first <- letter <|> symbol
                rest <- many $ letter <|> digit <|> symbol
                let atom = first:rest
                return $ case atom of
                            "#t" -> Bool True
                            "#f" -> Bool False
                            _    -> Atom atom

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseNumber 
         <|> parseAtom
         <|> parseString
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList  -- try is used here for backtracking
                char ')'
                return x
