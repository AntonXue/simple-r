module Main where

import Lib
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  _ <- case args of
    ("--parse" : dir : []) -> parseFile dir
    ("--parse-test" :  []) -> parseTest
    ("--run" : file : []) -> runFile file
    _ -> putStrLn "main: unrecognized command"
  -- prog <- parseRFile $ head args
  -- putStrLn $ show prog
  -- state <- loadFileGuessWithBase $ head args
  -- putStrLn $ ppState state

  putStrLn "main: done!"

runFile :: String -> IO ()
runFile file = do
  -- state <- loadFileGuessWithBase $ file
  state <- loadUserGuessWithDefaultBase $ file
  let redAcc = runN 100 state
  -- putStrLn $ ppHist ([], state)
  -- putStrLn $ show $ length $ compAcc acc
  putStrLn $ ppRedAccum redAcc
  putStrLn $ show $ getValuesRed redAcc


parseFile :: String -> IO ()
parseFile dir = do
  _ <- testPassesOnDir dir
  putStrLn "parseFile: done!"

testStr :: String
testStr = ""


parseTest :: IO ()
parseTest = do
  let res = read testStr :: RExpr
  putStrLn $ show res
  putStrLn "parseTest: done!"


