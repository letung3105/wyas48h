module Main where

import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )
-- accepted lisp symbols
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

-- read in the string and check if it is valid
-- `>>` binding is different with every monad
-- with Parsec, the parser parses and return its result
-- and passes the remaining string to the test parser
readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
  Left  err -> "No match: " ++ show err
  Right val -> "Found value"

-- recognize and skip one or more space
spaces :: Parser ()
spaces = skipMany1 space

main :: IO ()
main = do
  s <- getLine
  putStrLn $ readExpr s


