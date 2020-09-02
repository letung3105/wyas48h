module Main where

import           Data.Char
import           Numeric
import           System.Environment
import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

-- | Check if the give string is a valid expression
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> String $ "No match: " ++ show err
  Right val -> val

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
  show (Atom   a) = a
  show (Number n) = show n
  show (String s) = "\"" ++ s ++ "\""
  show (Bool   b) = if b then "#t" else "#f"
  show (Char   c) = case c of
    ' '  -> "#\\space"
    '\n' -> "#\\newline"
    _    -> "#\\" ++ [c]
  show (List l        ) = "(" ++ unwordsList l ++ ")"
  show (DottedList h t) = "(" ++ unwordsList h ++ "." ++ show t ++ ")"

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
eval :: LispVal -> LispVal
eval val@(String _                 ) = val
eval val@(Number _                 ) = val
eval val@(Bool   _                 ) = val
eval (    List   [Atom "quote", e] ) = e
eval (    List   (Atom func : args)) = apply func $ map eval args

-- | Apply the operand to the list of lisp values,
-- returns `Bool False` if there is an error
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) (lookup func primitives)

-- | List of primitives operands
primitives :: [(String, [LispVal] -> LispVal)]
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
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

-- | Attempt to get the value inside a lisp value
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) =
  let parsed = reads n in if null parsed then 0 else fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum _          = 0
