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
  state <- loadFileGuess $ file
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
testStr =
  "RBinOp (RAssignArrow) (RVar (RIdent {ridPkg = Nothing, ridName = \"S\", ridSrc = Nothing, ridAnnot = Nothing})) (RFunCall (RVar (RIdent {ridPkg = Nothing, ridName = \"switch\", ridSrc = Nothing, ridAnnot = Nothing})) [RExprArg (RVar (RIdent {ridPkg = Nothing, ridName = \"design\", ridSrc = Nothing, ridAnnot = Nothing})),RStringAssign (RString \"2\") (RConst (RNumConst (RNumInt 5))),RStringAssign (RString \"6\"),RStringAssign (RString \"7\"),RStringAssign (RString \"8\"),RStringAssign (RString \"9\"),RStringAssign (RString \"10\") (RConst (RNumConst (RNumInt 50))),RExprArg (RConst (RNumConst (RNumInt 10)))])"




parseTest :: IO ()
parseTest = do
  let res = read testStr :: RExpr
  putStrLn $ show res
  putStrLn "parseTest: done!"


