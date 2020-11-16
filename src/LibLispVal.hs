module LibLispVal
    ( LispVal(Atom, List, DottedList, Number, String, Bool, PrimitiveFunc, Func, IOFunc, Port)
    , unwordsList
    , LispError(NumArgs, TypeMismatch, Parser, BadSpecialForm, NotFunction, UnboundVar, Default)
    , ThrowsError
    , trapError
    , extractValue
    , IOThrowsError
    , Env
    , liftThrows
    , showVal
    , runIOThrows
    ) where

import Data.IORef
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)
import System.IO

type Env = IORef [(String, IORef LispVal)]

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

instance Show LispVal where show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents)      = "\"" ++ contents ++ "\""
showVal (Atom name)            = name
showVal (Number contents)      = show contents
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"
showVal (List contents)        = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++  ")"
showVal (PrimitiveFunc _)      = "<primitive>"
showVal (Port _)               = "<IO port>"
showVal (IOFunc _)             = "<IO primitives>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
    "(lambda (" ++ unwords (map show args) ++
        (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"

-- Error handling
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError

instance Show LispError where show = showError

showError :: LispError -> String
showError (UnboundVar message varname)   = message ++ ": " ++ varname
showError (BadSpecialForm message form)  = message ++ ": " ++ show form
showError (NotFunction  message func)    = message ++ ": " ++ show func
showError (NumArgs  excepted found)      = "Expected " ++ show excepted 
                                        ++ " args; found values " ++ unwordsList found
showError (TypeMismatch excepted found)  = "Invalid type: expected " ++ excepted
                                        ++ ", found " ++ show found
showError (Parser parseErr)              = "Parse error at " ++ show parseErr

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- Error functions for the Env
type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue
