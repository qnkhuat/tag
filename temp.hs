module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex
import Control.Monad.Except


-- | Data types
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Float Float
             | Character Char
             | Ratio Rational
             | Complex (Complex Double)
             | Bool Bool deriving (Show, Eq)

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String deriving (Show)

-- | Error handling
type ThrowsError = Either LispError
trapError action = catchError action (return . show)
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- | Parser
escapedChars :: Parser Char
escapedChars = do
            char '\\' -- a backslash
            x <- oneOf "\\\"nrt" 
            return $ case x of 
                        '\\' -> x
                        '"'  -> x
                        'n'  -> '\n'
                        'r'  -> '\r'
                        't'  -> '\t'

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ escapedChars <|> noneOf "\"\\"
    char '"'
    return $ String x


parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
                  "#t" -> Bool True
                  "#f" -> Bool False
                  _    -> Atom atom

parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit
                char '/'
                y <- many1 digit
                return $ Ratio ((read x) % (read y))

parseNumber :: Parser LispVal
parseNumber = parseDecimal1 
            <|> parseDecimal2 
            <|> parseHex 
            <|> parseOct 
            <|> parseBin

parseDecimal1 :: Parser LispVal
parseDecimal1 = many1 digit >>= (return . Number . read)

parseDecimal2 :: Parser LispVal
parseDecimal2 = do 
            try $ string "#d"
            x <- many1 digit
            (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do 
            try $ string "#x"
            x <- many1 hexDigit
            return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do 
            try $ string "#o"
            x <- many1 octDigit
            return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do 
            try $ string "#b"
            x <- many1 (oneOf "10")
            return $ Number (bin2dig x)


parseFloat :: Parser LispVal
parseFloat = do 
            x <- many1 digit
            char '.'
            y <- many1 digit
            return $ Float (fst.head$readFloat (x++"."++y))

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs
spaces :: Parser ()
spaces = skipMany1 space

toDouble :: LispVal -> Double
toDouble(Float f) = realToFrac f
toDouble(Number n) = fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do x <- (try parseFloat <|> parseDecimal1)
                  char '+'
                  y <- (try parseFloat <|> parseDecimal1)
                  char 'i'
                  return $ Complex (toDouble x :+ toDouble y)

parseList :: Parser LispVal
parseList = sepBy parseExpr spaces >>= return . List 

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
parseExpr = parseAtom
        <|> parseString
        <|> try parseFloat
        <|> try parseRatio
        <|> try parseComplex
        <|> try parseNumber -- we need the 'try' because
        <|> try parseBool -- these can all start with the hash char
        <|> try parseQuoted
        <|> do 
                char '('
                x <- try parseList  <|> parseDottedList
                char ')'
                return x

-- | Show
 
{-
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"

showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
instance Show LispVal where show = showValjjjjjjjjjjjjj

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError
-}

-- | Eval
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
                ("+", numericBinop (+)),
                ("-", numericBinop (-)),
                ("*", numericBinop (*)),
                ("/", numericBinop div),
                ("mod", numericBinop mod),
                ("quotient", numericBinop quot),
                ("remainder", numericBinop rem),
                ("?eq", compareBinop),
                ("symbol?" , unaryOp symbolp),
                ("string?" , unaryOp stringp),
                ("number?" , unaryOp numberp),
                ("bool?", unaryOp boolp),
                ("list?" , unaryOp listp),
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
                ("string>=?", strBoolBinop (>=))]

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return $ f v

symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _)   = Bool True
symbolp _          = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
boolp   (Bool _)   = Bool True
boolp   _          = Bool False
listp   (List _)   = Bool True
listp   (DottedList _ _) = Bool False
listp   _          = Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op

compareBinop :: [LispVal] -> ThrowsError LispVal
compareBinop (x:xs) = return $ Bool (all(==x) xs)
compareBinop _ = return $ Bool False

eval :: LispVal -> ThrowsError LispVal
eval val@(Atom _) = return val
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _)   = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
                      Left err -> throwError $ Parser err
                      Right val -> return val

main :: IO ()
main = do 
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled

