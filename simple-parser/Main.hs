module Main where

import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )
import           System.Environment
import           Data.Char
import           Numeric

main :: IO ()
main = do
  expr <- getLine
  putStrLn $ readExpr expr

-- | Check if the give string is a valid expression
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> "No match: " ++ show err
  Right val -> "Found value " ++ show val

run :: Show a => Parser a -> String -> String
run parser input = case parse parser "lisp" input of
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
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving (Eq, Show)

-- | Parse a lisp expression
parseExpr :: Parser LispVal
parseExpr = parseBool <|> parseString <|> parseNumber <|> parseAtom

-- | Parse a lisp atom
--
-- An atom is a letter or symbol, followed by any number of
-- letters, digits, or symbols
parseAtom :: Parser LispVal
parseAtom = do
  x  <- letter <|> symbol
  xs <- many $ letter <|> symbol <|> digit
  return $ Atom (x : xs)

parseBool :: Parser LispVal
parseBool = do
  v <- string "#t" <|> string "#f"
  return $ case v of
    "#t" -> Bool True
    "#f" -> Bool False

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
  oneOf "\\\"nrt"

-- | Parse a lisp number
parseNumber :: Parser LispVal
parseNumber = parseNumberWithRadix <|> parseDecimal

parseNumberWithRadix :: Parser LispVal
parseNumberWithRadix = do
  radix <- string "#o" <|> string "#b" <|> string "#d" <|> string "#x"
  case radix of
    "#b" -> parseBinary
    "#o" -> parseOctal
    "#d" -> parseDecimal
    "#x" -> parseHexadecimal

-- | Parse a number represented in binary
parseBinary :: Parser LispVal
parseBinary = do
  ns <- many1 $ oneOf "01"
  return $ Number $ binToInt ns
 where
  binToInt :: String -> Integer
  binToInt ns = toInteger $ sum
    [ bit * 2 ^ i | (bit, i) <- zip (map digitToInt ns) [ln - 1, ln - 2 ..] ]
    where ln = length ns

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
