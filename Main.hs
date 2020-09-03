module Main where

import           Data.Char
import           Exception               hiding ( try )
import           Numeric
import           System.Environment
import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )

main :: IO ()
-- main = undefined
main = do
  expr <- getLine
  let v = fmap show (readExpr expr >>= eval)
  case v of
    Left  err  -> print err
    Right expr -> print expr

-- | Check if the give string is a valid expression
readExpr :: String -> LispResult LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> throw $ Parser err
  Right val -> return val

-- | Lisp grammar
data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Char Char
  deriving (Eq)

instance Show LispVal where
  show (Atom a        ) = a
  show (List l        ) = "(" ++ unwordsList l ++ ")"
  show (DottedList h t) = "(" ++ unwordsList h ++ "." ++ show t ++ ")"
  show (Number n      ) = show n
  show (String s      ) = "\"" ++ s ++ "\""
  show (Bool   b      ) = if b then "#t" else "#f"
  show (Char   c      ) = case c of
    ' '  -> "#\\space"
    '\n' -> "#\\newline"
    _    -> "#\\" ++ [c]

-- | Parse a lisp symbol
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

-- | Parse one or more spaces
spaces :: Parser ()
spaces = skipMany1 space

-- | Parse escaped character sequence in a string
escapedChar :: Parser Char
escapedChar = do
  char '\\'
  oneOf "\\\"nrt"

-- | Turn a list of lisp values into a string where each of the value is seperated by a space
unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

-- | Parse a lisp expression
parseExpr :: Parser LispVal
parseExpr =
  try parseAtom
    <|> try parseBool
    <|> try parseNumber
    <|> parseChar
    <|> parseString
    <|> parseQuoted
    <|> do
          char '('
          x <- try parseList <|> parseDottedList
          char ')'
          return x

-- | Parse a lisp identifier
-- An atom is a letter or symbol, followed by any number of
-- letters, digits, or symbols
parseAtom :: Parser LispVal
parseAtom = do
  x <- letter <|> symbol
  case x of
    '#' -> pzero
    _   -> do
      xs <- many $ letter <|> symbol <|> digit
      return $ Atom $ x : xs

-- | Parse a lisp boolean
parseBool :: Parser LispVal
parseBool = do
  char '#'
  r <- oneOf "tf"
  return $ case r of
    't' -> Bool True
    'f' -> Bool False

-- | Parse a lisp string
-- A string begins with a double quote mark, followed by any number of
-- non-quote character, and ends with a double quote mark
parseString :: Parser LispVal
parseString = do
  char '"'
  v <- many $ escapedChar <|> noneOf "\""
  char '"'
  return $ String v

-- | Parse a lisp number
parseNumber :: Parser LispVal
parseNumber = parseWithRadix <|> parseDecimal

parseWithRadix :: Parser LispVal
parseWithRadix = do
  char '#'
  r <- oneOf "bodx"
  case r of
    'b' -> parseBinary
    'o' -> parseOctal
    'd' -> parseDecimal
    'x' -> parseHexadecimal

-- | Parse a number represented in binary
parseBinary :: Parser LispVal
parseBinary = do
  ns <- many1 $ oneOf "01"
  return $ Number $ foldl f 0 $ zip [0 ..] $ binToDigits ns
 where
  f :: Integer -> (Integer, Integer) -> Integer
  f sum (pos, bit) = sum + bit * 2 ^ pos

  binToDigits :: String -> [Integer]
  binToDigits ns = map (toInteger . digitToInt) ns

-- | Parse a number represented in octal
parseOctal :: Parser LispVal
parseOctal = do
  ns <- many1 octDigit
  case readOct ns of -- there should be no error
    [(n, _)] -> return $ Number n

-- | Parse a number represented in decimal
parseDecimal :: Parser LispVal
parseDecimal = do
  ns <- many1 digit
  case readDec ns of -- there should be no error
    [(n, _)] -> return $ Number n

-- | Parse a number represented in hexadecimal
parseHexadecimal :: Parser LispVal
parseHexadecimal = do
  ns <- many1 hexDigit
  case readHex ns of -- there should be no error
    [(n, _)] -> return $ Number n

-- | Parse lisp syntactic single quote
parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- | Parse lisp list
parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

-- | Parse lisp dotted list
parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces -- endBy = seperated and ended with
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

-- | Parse lisp character
parseChar :: Parser LispVal
parseChar = do
  string "#\\"
  c <- try characterName <|> anyToken
  return $ Char c
 where
  characterName :: Parser Char
  characterName = do
    name <- many1 letter
    case map toLower name of
      "space"   -> return ' '
      "newline" -> return '\n'
      _         -> pzero

-- | Evaluate lisp expression
eval :: LispVal -> LispResult LispVal
eval v@(String _                 ) = return v
eval v@(Number _                 ) = return v
eval v@(Bool   _                 ) = return v
eval (  List   [Atom "quote", e] ) = return e
eval (  List   (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throw $ BadSpecialForm "Unrecognized special form" badForm

-- | Apply the operand to the list of lisp values,
-- returns `Bool False` if there is an error
apply :: String -> [LispVal] -> LispResult LispVal
apply func args = maybe
  (throw $ NotFunction "Unrecognized primitive function" func)
  ($ args)
  (lookup func primitives)

-- | List of primitives operands
primitives :: [(String, [LispVal] -> LispResult LispVal)]
primitives =
  [ ("+"        , numericBinop (+))
  , ("-"        , numericBinop (-))
  , ("*"        , numericBinop (*))
  , ("/"        , numericBinop div)
  , ("mod"      , numericBinop mod)
  , ("quotient" , numericBinop quot)
  , ("remainder", numericBinop rem)
  ]

-- | Combine the list of numeric lisp values, with the given operator, into a single numeric lisp value
numericBinop
  :: (Integer -> Integer -> Integer) -> [LispVal] -> LispResult LispVal
numericBinop _  [e]    = throw $ NumArgs 2 [e]
numericBinop op params = Number . foldl1 op <$> mapM unpackNum params

-- | Attempt to get the value inside a lisp value
unpackNum :: LispVal -> LispResult Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n
  in  if null parsed
        then throw $ TypeMismatch "number" (String n)
        else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throw $ TypeMismatch "number" notNum


-- | Possible errors when parsing lisp
data LispException
    = Default String
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | NumArgs Integer [LispVal]
    | TypeMismatch String LispVal

instance Show LispException where
  show (Default msg            ) = msg
  show (Parser  paserErr       ) = "Parse error at " ++ show paserErr
  show (BadSpecialForm msg form) = msg ++ ": " ++ show form
  show (NotFunction    msg func) = msg ++ ": " ++ show func
  show (UnboundVar     msg name) = msg ++ ": " ++ show name
  show (NumArgs expected found) =
    "Expected " ++ show expected ++ " args: Found values " ++ unwordsList found
  show (TypeMismatch expected found) =
    "Invalid type: expected " ++ show expected ++ ", found " ++ show found

instance Exception LispException

type LispResult a = Either LispException a

-- | Get the correct path of either, `Left` is intentionally ommitted
extractValue :: LispResult a -> a
extractValue (Right v) = v
