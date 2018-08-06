module Main where

import Lib
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  -- prog <- parseRFile $ head args
  -- putStrLn $ show prog
  state <- loadFileGuessWithBase $ head args
  putStrLn $ show state

