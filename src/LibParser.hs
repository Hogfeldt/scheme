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

matchInteger :: Parser [Char]
matchInteger = many1 digit

matchFloat :: Parser [Char]
matchFloat = do
                x <- matchInteger
                char '.'
                y <- matchInteger
                return $ x ++ "." ++ y

matchSciPosFloat :: Parser [Char]
matchSciPosFloat = 
            do
                float <- matchFloat
                char 'e'
                z <- matchInteger
                return $ float ++ "e" ++ z

matchSciNegFloat :: Parser [Char]
matchSciNegFloat = 
            do
                float <- matchFloat
                char 'e'
                char '-'
                z <- matchInteger
                return $ float ++ "e-" ++ z

parseInteger :: Parser LispVal
parseInteger = liftM (Number . read) $ matchInteger

parseNegativeInteger :: Parser LispVal
parseNegativeInteger = 
            do
                char '-'
                x <- matchInteger
                return $ Number (read ("-" ++ x))

parseFloat :: Parser LispVal 
parseFloat = 
            do
                float <- matchFloat
                return $ Float (fst . head $ readFloat (float))

parseNegativeFloat :: Parser LispVal
parseNegativeFloat = 
            do
                char '-'
                float <- matchFloat
                return $ Float (fst . head $ readSigned readFloat ("-" ++ float))

parseSciNegFloat :: Parser LispVal 
parseSciNegFloat =
            do
                sciNegFloat <- matchSciNegFloat
                return $ Float (fst . head $ readFloat (sciNegFloat))

parseNegSciNegFloat :: Parser LispVal
parseNegSciNegFloat =
            do
                char '-'
                sciNegFloat <- matchSciNegFloat
                return $ Float (fst . head $ readSigned readFloat ("-" ++ sciNegFloat))

parseSciPosFloat :: Parser LispVal 
parseSciPosFloat = 
            do
                sciPosFloat <- matchSciPosFloat
                return $ Float (fst . head $ readFloat (sciPosFloat))

parseNegSciPosFloat :: Parser LispVal 
parseNegSciPosFloat = 
            do
                char '-'
                sciPosFloat <- matchSciPosFloat
                return $ Float (fst . head $ readSigned readFloat ("-" ++ sciPosFloat))

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

parseNumber :: Parser LispVal
parseNumber     = try parseSciNegFloat 
               <|> try parseSciPosFloat 
               <|> try parseNegSciPosFloat 
               <|> try parseNegSciNegFloat 
               <|> try parseNegativeFloat 
               <|> try parseFloat  
               <|> try parseNegativeInteger 
               <|> parseInteger

parseExpr :: Parser LispVal
parseExpr = parseNumber 
         <|> parseAtom
         <|> parseString
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList  -- try is used here for backtracking
                char ')'
                return x
