module SimpleR.Interpreter.Preprocessor.TestPasses
  ( testPassesOnDir
  ) where

import System.Directory
import Data.Char
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

bar :: String
bar = "********************************************************************"

testPassesOnDir :: String -> IO ()
testPassesOnDir dir = do
  -- Figure out which things to parse
  files <- getDirectoryContents dir
  let onlyRFiles = sortBy (\a b -> (map toUpper a) `compare` (map toUpper b)) $
                   filter (isSuffixOf ".R") files

  -- Load the Program(s)
  allProgs <- mapM progFromFile $ map (canonRFile dir) onlyRFiles
  let allPairs = zip onlyRFiles allProgs
  let numAllPairs = length allPairs
  let parsedPairs = filter (\(_, Program es) -> es /= []) allPairs
  let numParsedPairs = length parsedPairs

  -- Printing this makes IO force everything before to finish
  putStrLn ""
  putStrLn bar
  putStrLn $ "tpod: " ++ show numParsedPairs ++ "/" ++ show numAllPairs

  -- TEST: pure args
  putStrLn ""
  putStrLn bar
  let pureArgsPairs = map (\(f, p) -> (f, testPureArgCallFails p)) parsedPairs
  let numPureArgsPairs = length $ filter snd pureArgsPairs
  putStrLn $ "tpod: [pure args] " ++ show numPureArgsPairs ++ "/"
                                  ++ show numParsedPairs
  _ <- mapM (\(f, _) -> putStrLn $ "  " ++ f) $ filter snd pureArgsPairs

  -- TEST: rm calls
  putStrLn ""
  putStrLn bar
  let rmCallPairs = map (\(f, p) -> (f, testRmCallFails p)) parsedPairs
  let numRmCallPairs = length $ filter snd rmCallPairs
  putStrLn $ "tpod: [rm calls] " ++ show numRmCallPairs ++ "/"
                                 ++ show numParsedPairs
  _ <- mapM (\(f, _) -> putStrLn $ "  " ++ f) $ filter snd rmCallPairs

  -- TEST: objects used
  putStrLn ""
  putStrLn bar
  let objAttrPairs = map (\(f, p) -> (f, testObjAttrFails p)) parsedPairs
  let numObjAttrPairs = length $ filter snd objAttrPairs
  putStrLn $ "tpod: [obj attr] " ++ show numObjAttrPairs ++ "/"
                                 ++ show numParsedPairs
  _ <- mapM (\(f, _) -> putStrLn $ "  " ++ f) $ filter snd objAttrPairs


  putStrLn "********************************************"
  return ()

