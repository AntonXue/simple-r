module SimpleR.Interpreter.Preprocessor.TestPasses
  ( testPassesOnDir
  ) where

import System.Directory
import Data.List

import SimpleR.Language
import SimpleR.Interpreter.Natives
import SimpleR.Interpreter.Preprocessor.Passes
import SimpleR.Interpreter.Preprocessor.LinearizationFromFile
import SimpleR.Interpreter.Preprocessor.Loader

testPureArgCallFails :: Program -> Bool
testPureArgCallFails prog =
  case pureArgsPass prog of
    PassOkay _ -> False
    PassFail _ -> True

testRmCallFails :: Program -> Bool
testRmCallFails prog =
  case funCalledPass (idFromString "rm") prog of
    PassOkay _ -> False
    PassFail _ -> True

testObjAttrFails :: Program -> Bool
testObjAttrFails prog =
  case primUsedPass (idPrimFromString "@") prog of
    PassOkay _ -> False
    PassFail _ -> True

testPassesOnDir :: String -> IO ()
testPassesOnDir dir = do
  -- Figure out which things to parse
  files <- getDirectoryContents dir
  let onlyRFiles = filter (isSuffixOf ".R") files

  -- Load the Program(s)
  allProgs <- mapM progFromFile $ map (canonRFile dir) onlyRFiles
  let allPairs = zip onlyRFiles allProgs
  let numAllPairs = length allPairs
  let parsedPairs = filter (\(_, Program es) -> es /= []) allPairs
  let numParsedPairs = length parsedPairs

  -- Printing this makes IO force everything before to finish
  putStrLn ""
  putStrLn "********************************************"
  putStrLn $ "tpod: " ++ show numParsedPairs ++ "/" ++ show numAllPairs

  -- TEST: pure args
  let pureArgsPairs = map (\(f, p) -> (f, testPureArgCallFails p)) parsedPairs
  let numPureArgsPairs = length $ filter snd pureArgsPairs
  putStrLn $ "tpod: [pure args] " ++ show numPureArgsPairs ++ "/"
                                  ++ show numParsedPairs

  -- TEST: rm calls
  let rmCallPairs = map (\(f, p) -> (f, testRmCallFails p)) parsedPairs
  let numRmCallPairs = length $ filter snd rmCallPairs
  putStrLn $ "tpod: [rm calls] " ++ show numRmCallPairs ++ "/"
                                 ++ show numParsedPairs

  -- TEST: objects used
  let objUsedPairs = map (\(f, p) -> (f, testObjAttrFails p)) parsedPairs
  let numObjAttrPairs = length $ filter snd objUsedPairs
  putStrLn $ "tpod: [obj attr] " ++ show numObjAttrPairs ++ "/"
                                 ++ show numParsedPairs



  putStrLn "********************************************"
  return ()

