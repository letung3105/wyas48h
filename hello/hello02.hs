module Main where

import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  print $ sum $ map read args
