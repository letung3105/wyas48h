module Main where

import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpr $ head args

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
parseExpr = parseAtom <|> parseString <|> parseNumber

-- | Parse a lisp string
--
-- A string begins with a double quote mark, followed by any number of
-- non-quote character, and ends with a double quote mark
parseString :: Parser LispVal
parseString = do
  char '"'
  v <- many $ noneOf "\""
  char '"'
  return $ String v

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

-- | Parse a lisp number
parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit
