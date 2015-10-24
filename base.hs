{-# LANGUAGE ExistentialQuantification #-}

import Data.Ratio
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Applicative hiding ((<|>), many)
import System.Environment
import Data.Char
import Data.Complex
import Numeric
import System.IO
import Data.IORef


data MyNumber = Integer Integer
              | Rational (Ratio Integer)
              | Float Double deriving Eq

instance Show MyNumber where show = showNum
showNum :: MyNumber -> String
showNum x = case x of
  Integer i  -> show i
  Rational i -> show i
  Float i    -> show i


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Complex (Complex Double)
             | Number MyNumber
             | String String
             | Character Char
             | Bool Bool
             | Port Handle
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Func { params   :: [String]
                    , vararg   :: Maybe String
                    , body     :: [LispVal]
                    , closure  :: Env
                    }

instance Show LispVal where show = showVal
showVal :: LispVal -> String
showVal lv = case lv of
  Atom x                -> x
  String s              -> s
  Character x           -> [x]
  Number x              -> show x
  Complex x             -> show x
  Bool True             -> "#t"
  Bool False            -> "#f"
  List xs               -> "(" ++ unwordsList xs ++ ")"
  DottedList hd tl      -> "(" ++ unwordsList hd ++ showVal tl ++ ")"
  PrimitiveFunc _       -> "<primitive>"
  Port _                -> "<IO port>"
  IOFunc _              -> "<IO primitive"
  Func { params = args
       , vararg = varargs
       , body = _
       , closure = _} -> "(lambda (" ++ unwords (map show args) ++ showVar varargs ++ ") ...)"
  where
    showVar varargs = case varargs of
        Nothing  -> ""
        Just arg -> " . " ++ arg

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
instance Error LispError where
    noMsg  = Default "An error has occurred"
    strMsg = Default
instance Show LispError where show = showError
showError :: LispError -> String
showError x = case x of
    UnboundVar message varname  -> message ++ ": " ++ varname
    BadSpecialForm message form -> message ++ ": " ++ show form
    NotFunction message func    -> message ++ ": " ++ show func
    NumArgs expected found      -> "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
    TypeMismatch expected found -> "Invalid type: expected " ++ expected ++ ", found " ++ show found
    Parser parseErr             -> "Parse error at " ++ show parseErr
    Default message             -> message

type ThrowsError = Either LispError

--- trapError :: ThrowsError String -> ThrowsError String
trapError action = catchError action (return . show)

extractValue :: ThrowsError t -> t
extractValue (Right val) = val
extractValue (Left val) = error $ "no good: " ++ show val

------ Parsing ------

parseChar :: Parser LispVal
parseChar = do
  _ <- try $ string "#\\"
  c <- try parseW <|> parseC
  return . Character . stoc $ c
  where
    parseW = string "newline" <|> string "space"
    parseC = fmap pure anyChar
    stoc "space" = ' '
    stoc "newline" = '\n'
    stoc x = head x

parseBool :: Parser LispVal
parseBool = fmap (Bool . (== 't')) (char '#' >> oneOf "tf")

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (escapedQuote <|> noneOf "\"")
  _ <- char '"'
  return . String $ x
  where escapedQuote = char '\\' >> (char '"' <|> char 'n' <|> char 'r' <|> char '\\')


parseComplex :: Parser LispVal
parseComplex = liftM4 f parseMyNumber (char '+') parseMyNumber (char 'i')
  where
    f a _ c _ = Complex $ td a :+ td c
    td n = case n of
      Integer  i -> fromIntegral i
      Rational x -> (fromIntegral . numerator $ x) / (fromIntegral . denominator $ x)
      Float    d -> d

parseNumber :: Parser LispVal
parseNumber = liftM Number parseMyNumber

parseMyNumber :: Parser MyNumber
parseMyNumber = try parseFloat <|> try parseRational <|> parseInteger
  where
    parseInteger = try parseHex <|> try parseOct <|> try parseDec <|> try parseBin <|> parseDef
      where
        parseDef = liftM (Integer . read) $ many1 digit
        parseDec = parseNumType 'd' readDec
        parseHex = parseNumType 'x' readHex
        parseOct = parseNumType 'o' readOct
        parseBin = parseNumType 'b' $ readInt 2 (`elem` "01") digitToInt
        parseNumType sig reader = do
          _ <- try $ string ['#', sig]
          x <- many1 digit
          return . Integer . fst . head . reader $ x

    parseFloat = liftM3 f (many1 digit) (char '.') (many1 digit)
      where f a b c = Float . fst . head . readFloat $ a ++ [b] ++ c

    parseRational = liftM3 f (many1 digit) (char '/') (many1 digit)
      where f a _ b = Rational (read a % read b)

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return . Atom $ atom
  where symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  hd <- endBy parseExpr spaces
  tl <- char '.' >> spaces >> parseExpr
  return $ DottedList hd tl

parseAnyList :: Parser LispVal
parseAnyList = do
  _ <- char '('
  x <- try parseList <|> try parseDottedList
  _ <- char ')'
  return x

parseSpecialQuote :: Char -> String -> Parser LispVal
parseSpecialQuote c special = fmap (List . ([Atom special] ++) . pure) (char c >> parseExpr)

parseQuote :: Parser LispVal
parseQuote = parseSpecialQuote '\'' "quote"

parseQuasiQuote :: Parser LispVal
parseQuasiQuote = parseSpecialQuote '`' "quasiquote"

parseUnquote :: Parser LispVal
parseUnquote = parseSpecialQuote ',' "unquote"

parseExpr :: Parser LispVal
parseExpr = try parseComplex
  <|> try parseNumber
  <|> parseQuote
  <|> parseQuasiQuote
  <|> parseUnquote
  <|> parseAnyList
  <|> parseAtom
  <|> parseString
  <|> parseChar
  <|> parseBool

spaces :: Parser ()
spaces = skipMany1 space

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)


------------ Evaluaton ---------------

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val = case val of
  String _                             -> return val
  Number _                             -> return val
  Bool _                               -> return val
  Character _                          -> return val
  Atom var                             -> getVar env var
  List [Atom "quote", v]               -> return v
  List [Atom "if", prop, conseq, alt]  -> eval env prop >>= iffun conseq alt
  List ((Atom "cond"):clauses)         -> cond clauses
  List ((Atom "case"):key:clauses)     -> mycase key clauses
  List [Atom "set!", Atom var, form]   -> eval env form >>= setVar env var
  List [Atom "define", Atom var, form] -> eval env form >>= defineVar env var
  List [Atom "load", String filename]  -> load filename >>= liftM last . mapM (eval env)

  List (Atom "define" : List (Atom var : params) : body)               ->
    makeNormalFunc env params body >>= defineVar env var
  List (Atom "define" : DottedList (Atom var : params) varargs : body) ->
    makeVarArgs varargs env params body >>= defineVar env var
  List (Atom "lambda" : List params : body)                            ->
    makeNormalFunc env params body
  List (Atom "lambda" : DottedList params varargs : body)              ->
    makeVarArgs varargs env params body
  List (Atom "lambda" : varargs@(Atom _) : body)                       ->
    makeVarArgs varargs env [] body
  List (function : args)                                               ->
    do
        func <- eval env function
        argVals <- mapM (eval env) args
        apply func argVals

  List _                               -> return val
  _                                    -> throwError $ BadSpecialForm "Unrecognized special form" val
  where
    iffun conseq alt lv = case lv of
        Bool False -> eval env alt
        Bool True  -> eval env conseq
        f          -> throwError $ TypeMismatch "boolean" f

    mycase :: LispVal -> [LispVal] -> IOThrowsError LispVal
    mycase key ((List ((Atom "else"):expressions)):clauses) = liftM last $ mapM (eval env) expressions
    mycase key ((List ((List datums):expressions)):clauses) = do
        lkey  <- eval env key
        match <- anyeq lkey datums
        if match
            then liftM last $ mapM (eval env) expressions
            else mycase key clauses
        where
            anyeq _ [] = return False
            anyeq k (x:xs) = do
                a <- liftThrows $ eqv [k, x]
                case a of
                    Bool True -> return True
                    _         -> anyeq k xs

    cond :: [LispVal] -> IOThrowsError LispVal
    cond ((List ((Atom "else"):expressions)):clauses) = liftM last $ mapM (eval env) expressions
    cond ((List (test:expressions)):clauses) = do
        lpred <- eval env test
        bpred  <- extractBool lpred
        if bpred
            then liftM last $ mapM (eval env) expressions
            else cond clauses
    extractBool :: LispVal -> IOThrowsError Bool
    extractBool lv = case lv of
        Bool v -> return v
        f      -> throwError $ TypeMismatch "boolean" f
    makeFunc varargs e params body = return $ Func (map showVal params) varargs body e
    makeNormalFunc = makeFunc Nothing
    makeVarArgs = makeFunc . Just . showVal

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (IOFunc func) args = func args
apply (PrimitiveFunc f) args = liftThrows $ f args
apply (Func params varargs body closure) args =
    if num params /= num args && varargs == Nothing
        then throwError $ NumArgs (num params) args
        else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where
        remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
            Nothing -> return env

---------- Primitives ---------
funs :: [(String, [LispVal] -> Either LispError LispVal)]
funs =
  [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)

  , ("boolean?", unaryOp isBool)
  , ("symbol?", unaryOp isAtom)
  , ("string?", unaryOp isString)
  , ("number?", unaryOp isNum)
  , ("list?", unaryOp isList)
  , ("symbol->string", unaryOp symbolToString)
  , ("string->symbol", unaryOp stringToSymbol)

  , ("=", numBoolBinop (==))
  , ("<", numBoolBinop (<))
  , (">", numBoolBinop (>))
  , ("/=", numBoolBinop (/=))
  , (">=", numBoolBinop (>=))
  , ("<=", numBoolBinop (<=))

  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))

  , ("string=?", strBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))

  , ("string-ci=?", strBoolBinop  $ loweredOp (==))
  , ("string-ci<?", strBoolBinop  $ loweredOp (<))
  , ("string-ci>?", strBoolBinop  $ loweredOp (>))
  , ("string-ci<=?", strBoolBinop $ loweredOp (<=))
  , ("string-ci>=?", strBoolBinop $ loweredOp (>=))

  , ("make-string", makeString)
  , ("string", ctos)
  , ("string-append", stringAppend)
  , ("string-ref", stringRef)
  , ("substring", substring)
  , ("string-copy", unaryStringOp stringCopy)
  , ("string-length", unaryStringOp stringLength)
  , ("string->list", unaryStringOp stringToList)
  , ("list->string", listToString)

  --- , ("cond", cond)

  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)

  , ("eq?", eqv)
  , ("eqv?", eqv)
  , ("equal?", equal)

  ]
  where
    strBoolBinop  = boolBinop unpackStr
    boolBoolBinop = boolBinop unpackBool
    numBoolBinop  = boolBinop unpackNum
    loweredOp :: (String -> String -> Bool) -> String -> String -> Bool
    loweredOp op s1 s2 = op (map toLower s1) (map toLower s2)

boolBinop :: (LispVal -> ThrowsError t) -> (t -> t -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op xs = case xs of
    [x, y] -> liftM Bool (liftM2 op (unpacker x) (unpacker y))
    _      -> throwError $ NumArgs 2 xs

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp op xs = case xs of
    []   -> throwError $ NumArgs 1 []
    [x]  -> return . op $ x
    _    -> throwError $ NumArgs (fromIntegral . length $ xs) xs


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op xs = case xs of
    []  -> throwError $ NumArgs 2 []
    [x] -> throwError $ NumArgs 2 [x]
    _   -> liftM (Number . Integer . foldl1 op) (mapM unpackNum xs)

--- unpacking ---

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals a b (AnyUnpacker unpacker) = catchError
    (liftM2 (==) (unpacker a) (unpacker b))
    (const $ return False)

unpackBool :: LispVal -> ThrowsError Bool
unpackBool lv = case lv of
    Bool b  -> return b
    _       -> throwError $ TypeMismatch "boolean" lv

unpackStr :: LispVal -> ThrowsError String
unpackStr lv = case lv of
    String n           -> return n
    Number n           -> return . show $ n
    Bool s             -> return . show $ s
    _                  -> throwError $ TypeMismatch "string" lv

unpackNum :: LispVal -> ThrowsError Integer
unpackNum lv = case lv of
    Number (Integer z) -> return z
    List [n]           -> unpackNum n
    String n           -> stoi n
    notNum             -> throwError $ TypeMismatch "number" notNum
    where
        stoi n = case reads n of
            []         -> throwError $ TypeMismatch "number" $ String n
            x:_        -> return . fst $ x

eqv :: [LispVal] -> ThrowsError LispVal
eqv args = case args of
    [Bool x, Bool y]                       -> return . Bool $ x == y
    [Number x, Number y]                   -> return . Bool $ x == y
    [String x, String y]                   -> return . Bool $ x == y
    [Atom x, Atom y]                       -> return . Bool $ x == y
    [DottedList xs x, DottedList ys y]     -> eqv [List $ xs ++ [x], List $ ys ++ [y]]
    [List xs, List ys]                     -> equalLists eqv xs ys
    [_,_]                                  -> return . Bool $ False
    _                                      -> throwError $ NumArgs 2 args

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] =
    do
        primitiveEquals <- liftM or anyequals
        eqvEquals       <- eqhelp
        return $ Bool $ primitiveEquals || extract eqvEquals
    where
        unpacks = [ AnyUnpacker unpackNum , AnyUnpacker unpackStr , AnyUnpacker unpackBool]
        anyequals = mapM (unpackEquals arg1 arg2) unpacks
        extract (Bool x) = x
        extract _ = False
        eqhelp = case [arg1, arg2] of
            [List xs, List ys] -> equalLists equal xs ys
            _                  -> eqv [arg1, arg2]
equal badArgs = throwError $ NumArgs 2 badArgs

equalLists :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> [LispVal] -> ThrowsError LispVal
equalLists eqfun xs ys = return . Bool $ (length xs == length ys) && (all eqvPair $ zip xs ys)
    where
        eqvPair (x,y) = case eqfun [x,y] of
            (Right (Bool val)) -> val
            _                -> False

--------------- Lists ------------
car :: [LispVal] -> ThrowsError LispVal
car args = case args of
    [List (x: _)]         -> return x
    [DottedList (x:_) _]  -> return x
    [badArg]              -> throwError $ TypeMismatch "pair" badArg
    badArgList            -> throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr args = case args of
    [List (_: xs)]        -> return . List $ xs
    [DottedList [_] x]    -> return x
    [DottedList (_:xs) x] -> return $ DottedList xs x
    [badArg]              -> throwError $ TypeMismatch "pair" badArg
    badArgList            -> throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons args = case args of
    [x, List []]             -> return . List $ [x]
    [x, List xs]             -> return . List $ x:xs
    [x, DottedList xs xlast] -> return $ DottedList (x : xs) xlast
    [x, y]                   -> return $ DottedList [x] y
    bad                      -> throwError $ NumArgs 2 bad


--------------- String ------------------

unaryStringOp :: (String -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryStringOp op args = case args of
    [String s] -> return . op $ s
    [bad]      -> throwError $ TypeMismatch "string" bad
    badList    -> throwError $ NumArgs 1 badList

makeString :: [LispVal] -> ThrowsError LispVal
makeString args = case args of
    [Number (Integer k), Character c] -> return . String . replicate (fromIntegral k) $ c
    [Number (Integer k)]              -> return . String . replicate (fromIntegral k) $ ' '
    [bad]                             -> throwError $ TypeMismatch "int/char" bad
    badArgList                        -> throwError $ NumArgs 1 badArgList

ctos :: [LispVal] -> ThrowsError LispVal
ctos args = case args of
    []               ->return $ String ""
    Character c : cs -> ctos cs >>= lappend (String [c])
    (bad:_)          ->  throwError $ TypeMismatch "char" bad
    where
        lappend (String s1) (String s2) = return . String $ s1 ++ s2
        lappend x _ = throwError $ TypeMismatch "string" x

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef args = case args of
    [String s, Number (Integer k)] -> return . Character $ s !! fromIntegral k
    [bad]                          -> throwError $ TypeMismatch "string/int" bad
    badList                        -> throwError $ NumArgs 2 badList

substring :: [LispVal] -> ThrowsError LispVal
substring args = case args of
    [String s, Number (Integer start), Number (Integer end)] -> return . String . take (fromIntegral end - fromIntegral start) . drop (fromIntegral start) $ s
    [bad]                          -> throwError $ TypeMismatch "string/int/int" bad
    badList                        -> throwError $ NumArgs 2 badList

stringAppend :: [LispVal] -> ThrowsError LispVal
stringAppend args = case args of
    [String s, String s'] -> return . String $ s ++ s'
    [bad]                 -> throwError $ TypeMismatch "string/string" bad
    badList               -> throwError $ NumArgs 2 badList

listToString :: [LispVal] -> ThrowsError LispVal
listToString args = case args of
    [List cs] -> ctos cs
    [bad]     -> throwError $ TypeMismatch "list[char]" bad
    badList   -> throwError $ NumArgs 1 badList


stringLength :: String -> LispVal
stringLength = Number . Integer . fromIntegral . length

stringToList :: String -> LispVal
stringToList = List . map Character

stringCopy :: String -> LispVal
stringCopy = String

symbolToString :: LispVal -> LispVal
symbolToString (Atom s)   =  String s
symbolToString _          =  error "not an Atom"

stringToSymbol :: LispVal -> LispVal
stringToSymbol (String s) =  Atom s
stringToSymbol _          =  error "not a String"


-------------- Type tests ---------------

isBool :: LispVal   -> LispVal
isBool (Bool _)     =  Bool True
isBool _            =  Bool False

isAtom :: LispVal   -> LispVal
isAtom (Atom _)     =  Bool True
isAtom _            =  Bool False

isString :: LispVal -> LispVal
isString (String _) =  Bool True
isString _          =  Bool False

isNum :: LispVal    -> LispVal
isNum (Number _)    =  Bool True
isNum _             =  Bool False

isList :: LispVal   -> LispVal
isList (List _)     =  Bool True
isList _            =  Bool False


-------------- Mutation ----------------
type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ErrorT LispError IO

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = liftM extractValue (runErrorT (trapError action))

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe
        (throwError $ UnboundVar "unbound var" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe
        (throwError $ UnboundVar "assinging unbound var" var)
        (liftIO . (flip writeIORef value))
        (lookup var env)
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = liftIO (isBound envRef var) >>= defOrSet envRef var value
    where
        defOrSet envRef' var' value' existing
            |  existing = setVar envRef' var' value'
            | not existing = defvar envRef' var' value'
            | otherwise = error "what"
        defvar envRef' var' value' = liftIO $ do
            valueRef <- newIORef value'
            env <- readIORef envRef'
            writeIORef envRef' ((var', valueRef) : env)
            return value'

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = do
    env   <- readIORef envRef
    added <- extendEnv env
    newIORef added
    where
        extendEnv env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do
            ref <- newIORef value
            return (var, ref)


---------- I/O  ----------------
ioFuns :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioFuns =
    [ ("apply", applyProc)
    , ("open-input-file", makePort ReadMode)
    , ("open-output-file", makePort WriteMode)
    , ("close-input-port", closePort)
    , ("close-output-port", closePort)
    , ("read", readProc)
    , ("write", writeProc)
    , ("read-contents", readContents)
    , ("read-all", readAll)
    ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc args = case args of
    [func, List params] -> apply func params
    func:params         -> apply func params

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: FilePath -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

---------- REPL -----------------

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ ioPrimitiveFuns ++ primitiveFuns)
    where
        ioPrimitiveFuns         = map (makeFunc IOFunc) ioFuns
        primitiveFuns           = map (makeFunc PrimitiveFunc) funs
        makeFunc fc (var, func) = (var, fc func)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (t -> Bool) -> m t -> (t -> m ()) -> m ()
until_ pred' prompt action = do
    result <- prompt
    if pred' result
        then return ()
        else action result >> until_ pred' prompt action

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "lisp>>> ") . evalAndPrint

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) >>= hPutStrLn stderr

main :: IO ()
main = do
    args <- getArgs
    if null args then runRepl else runOne $ args
