{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module LibEval
    ( eval
    , primitiveBindings
    , readExpr
    , readExprList
    ) where

import Control.Monad.Except
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)

import LibLispVal
import LibEnv
import LibParser

boolMonop :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
boolMonop op args = if length args /= 1
                                then throwError $ NumArgs 1 args
                                else do return $ Bool (op (args !! 0))

numBoolMonop :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolMonop op params = case integerFloatOrMix params of
    IsDouble -> boolMonop op params
    IsInteger -> boolMonop op params
    IsMix -> throwError $ NumArgs 1 params

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                                then throwError $ NumArgs 2 args
                                else do left <- unpacker $ args !! 0
                                        right <- unpacker $ args !! 1
                                        return $ Bool $ left `op` right


numBoolBinop :: (forall a. (Eq a, Ord a) => a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop func params = case integerFloatOrMix params of
    IsDouble -> boolBinop unpackFloat func params
    IsMix -> boolBinop unpackFloat func params
    IsInteger -> boolBinop unpackNum func params

strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackFloat :: LispVal -> ThrowsError Double
unpackFloat (Float n)   = return n
unpackFloat (Number n)  = return $ fromIntegral n
unpackFloat (List [n])  = unpackFloat n
unpackFloat notNum      = throwError $ TypeMismatch "number" notNum

--Kunne have brugt where til IntegerFloatOrMixHelper
data NumericListType = IsInteger | IsDouble | IsMix

integerFloatOrMix :: [LispVal] -> NumericListType
integerFloatOrMix (Number _:xs)    = integerFloatOrMixHelper IsInteger xs
integerFloatOrMix (Float _:xs)     = integerFloatOrMixHelper IsDouble xs

integerFloatOrMixHelper :: NumericListType -> [LispVal] -> NumericListType
integerFloatOrMixHelper IsInteger []         = IsInteger
integerFloatOrMixHelper IsDouble []          = IsDouble
integerFloatOrMixHelper IsInteger (Number _:xs)   = integerFloatOrMixHelper IsInteger xs
integerFloatOrMixHelper IsDouble (Float _:xs)     = integerFloatOrMixHelper IsDouble xs
integerFloatOrMixHelper _ _                          = IsMix

numericBinop :: (forall a. Num a => a -> a -> a) -> [LispVal] -> ThrowsError LispVal
numericBinop op            [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal 
numericBinop op params        = case integerFloatOrMix params of
    IsInteger -> mapM unpackNum params >>= return . Number . foldl1 op
    IsDouble -> mapM unpackFloat params >>= return . Float . foldl1 op
    IsMix -> mapM unpackFloat params >>= return . Float . foldl1 op

floatingBinop :: (forall a. Floating a => a -> a -> a) -> [LispVal] -> ThrowsError LispVal
floatingBinop op            [] = throwError $ NumArgs 2 []
floatingBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal 
floatingBinop op params        = case integerFloatOrMix params of
    IsInteger -> mapM unpackFloat params >>= return . Float . foldl1 op
    IsDouble -> mapM unpackFloat params >>= return . Float . foldl1 op
    IsMix -> mapM unpackFloat params >>= return . Float . foldl1 op

fractionalBinop :: (forall a. Fractional a => a -> a -> a) -> [LispVal] -> ThrowsError LispVal
fractionalBinop op            [] = throwError $ NumArgs 2 []
fractionalBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal 
fractionalBinop op params        = case integerFloatOrMix params of
    IsInteger -> mapM unpackFloat params >>= return . Float . foldl1 op
    IsDouble -> mapM unpackFloat params >>= return . Float . foldl1 op
    IsMix -> mapM unpackFloat params >>= return . Float . foldl1 op

integralBinop :: (forall a. Integral a => a -> a -> a) -> [LispVal] -> ThrowsError LispVal
integralBinop op            [] = throwError $ NumArgs 2 []
integralBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal 
integralBinop op params        = case integerFloatOrMix params of
    IsInteger -> mapM unpackNum params >>= return . Number . foldl1 op
    _ -> throwError $ TypeMismatch "Integral" $ String "double"

numericMonop :: (forall a. Num a => a -> a) -> [LispVal] -> ThrowsError LispVal
numericMonop op []                          = throwError $ NumArgs 1 []
numericMonop op singleVal@[Number val]      = return . Float $ op $ fromIntegral val
numericMonop op singleVal@[Float val]       = return . Float $ op val
numericMonop op badParams                   = throwError $ TypeMismatch "Number" $ String "List"

floatingMonop :: (forall a. Floating a => a -> a) -> [LispVal] -> ThrowsError LispVal
floatingMonop op []                         = throwError $ NumArgs 1 []
floatingMonop op singleVal@[Number val]     = return . Float $ op $ fromIntegral val
floatingMonop op singleVal@[Float val]      = return . Float $ op val
floatingMonop op badParams                  = throwError $ TypeMismatch "Number" $ String "List"

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

isInt :: LispVal -> Bool
isInt (Number _) = True
isInt _ = False

isNumber :: LispVal -> Bool
isNumber (Float _) = True
isNumber (Number _) = True
isNumber _ = False

-- helper to eqv
listEqv l1 l2 = (length l1 == length l2) && 
    (all eqvPair $ zip l1 l2)
        where eqvPair (l1,l2) = case eqv [l1, l2] of
                                    Left err -> False
                                    Right (Bool val) -> val

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ listEqv arg1 arg2
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2 
            `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                       [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", fractionalBinop (/)),
              ("mod", integralBinop mod),
              ("quotient", integralBinop quot),
              ("remainder", integralBinop rem),
              ("log", floatingBinop logBase),
              ("sqrt", floatingMonop sqrt),
              ("ln", floatingMonop log),
              ("^", floatingBinop (**)),
              ("sin", floatingMonop sin),
              ("cos", floatingMonop cos),
              ("tan", floatingMonop tan),
              ("asin", floatingMonop asin),
              ("acos", floatingMonop acos),
              ("atan", floatingMonop atan),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("number?", numBoolMonop isNumber),
              ("integer?", numBoolMonop isInt),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool False)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args        = func args
apply (Func params varargs body closure) args =
    if num params /= num args && varargs == Nothing
        then throwError $ NumArgs (num params) args
        else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body
          bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
            Nothing -> return env

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                               ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor func)

-- Helpers to eval
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Float _)  = return val
eval env val@(Number _) = return val
eval env val@(Bool _)   = return val
eval env (Atom id)      = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
    do result <- eval env pred
       case result of
            Bool False -> eval env alt
            Bool True -> eval env conseq
            otherwise  -> throwError $ TypeMismatch "predicat to evaluate to a boolean type" otherwise
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) = 
    makeVarArgs varargs env [] body
eval env (List [Atom "load", String filename]) =
    load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)
