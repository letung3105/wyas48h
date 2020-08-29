module Main where

import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )
import           System.Environment
import           Numeric
import           Data.Char

main :: IO ()
main = do
  expr <- getLine
  putStrLn $ readExpr expr

-- | Check if the give string is a valid expression
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> "No match: " ++ show err
  Right val -> "Found value " ++ show val


-- | Parse a lisp symbol
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

-- | Parse one or more spaces
spaces :: Parser ()
spaces = skipMany1 space

-- | Lisp grammar
data LispVal = Atom String
             | String String
             | Number Integer
             | Bool Bool
             | List [LispVal]
             | DottedList [LispVal] LispVal
             deriving (Eq, Show)

-- | Parse a lisp expression
parseExpr :: Parser LispVal
parseExpr = parseNumber <|> parseString <|> parseAtom

-- | Parse a lisp atom
--
-- An atom is a letter or symbol, followed by any number of
-- letters, digits, or symbols
parseAtom :: Parser LispVal
parseAtom = do
  x  <- letter <|> symbol
  xs <- many $ letter <|> symbol <|> digit
  return $ case x : xs of
    "#t" -> Bool True
    "#f" -> Bool False
    atom -> Atom atom

-- | Parse a lisp string
--
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
  esc <- oneOf "\\\"nrt"
  return $ case esc of
    '\\' -> '\\'
    '"'  -> '"'
    'n'  -> '\n'
    'r'  -> '\r'
    't'  -> '\t'

-- | Parse a lisp number
parseNumber :: Parser LispVal
parseNumber = withRadix <|> decLiteral

-- | Parse number literals that are represented in other bases
withRadix :: Parser LispVal
withRadix = do
  char '#'
  radix <- oneOf "bodx"
  case radix of
    'b' -> binLiteral
    'o' -> octLiteral
    'd' -> decLiteral
    'x' -> hexLiteral

-- | Parse a number represented in binary
binLiteral :: Parser LispVal
binLiteral = do
  ns <- many1 $ oneOf "01"
  return $ Number $ binToInt ns
 where
  binToInt :: String -> Integer
  binToInt ns = toInteger $ sum
    [ bit * 2 ^ i | (bit, i) <- zip (map digitToInt ns) [ln - 1, ln - 2 ..] ]
    where ln = length ns

-- | Parse a number represented in octal
octLiteral :: Parser LispVal
octLiteral = do
  ns <- many1 octDigit
  case readOct ns of -- there should be no error
    [(n, _)] -> return $ Number n

-- | Parse a number represented in decimal
decLiteral :: Parser LispVal
decLiteral = do
  ns <- many1 digit
  case readDec ns of -- there should be no error
    [(n, _)] -> return $ Number n

-- | Parse a number represented in hexadecimal
hexLiteral :: Parser LispVal
hexLiteral = do
  ns <- many1 hexDigit
  case readHex ns of -- there should be no error
    [(n, _)] -> return $ Number n

