module Main where

import           Data.Char
import           Numeric
import           System.Environment
import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )

main :: IO ()
main = do
  expr <- getLine
  putStrLn $ readExpr expr

-- | Check if the give string is a valid expression
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> "No match: " ++ show err
  Right val -> "Found " ++ show val

-- | Parse a lisp symbol
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

-- | Parse one or more spaces
spaces :: Parser ()
spaces = skipMany1 space

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
  show = showVal

-- | Parse a lisp expression
parseExpr :: Parser LispVal
parseExpr =
  try parseAtom
    <|> try parseBool
    <|> try parseNumber
    <|> parseChar
    <|> parseString
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

-- | Parse escaped character sequence in a string
escapedChar :: Parser Char
escapedChar = do
  char '\\'
  oneOf "\\\"nrt"

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

parseQuote :: Parser LispVal
parseQuote = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces -- endBy = seperated and ended with
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

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

showVal :: LispVal -> String
showVal (Atom   a) = a
showVal (Number n) = show n
showVal (String s) = "\"" ++ s ++ "\""
showVal (Bool   b) = if b then "#t" else "#f"
showVal (Char   c) = case c of
  ' '  -> "#\\space"
  '\n' -> "#\\newline"
  _    -> "#\\" ++ [c]
showVal (List l        ) = "(" ++ unwordsList l ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ "." ++ show t ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
