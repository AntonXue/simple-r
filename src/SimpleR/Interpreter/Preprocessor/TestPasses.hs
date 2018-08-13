module SimpleR.Interpreter.Preprocessor.TestPasses
  ( testPassesOnDir
  ) where

import System.Directory
import Data.List

import SimpleR.Language
import SimpleR.Interpreter.Preprocessor.Passes
import SimpleR.Interpreter.Preprocessor.LinearizationFromFile
import SimpleR.Interpreter.Preprocessor.Loader

testPureArgCallsFails :: Program -> Bool
testPureArgCallsFails prog =
  case pureArgsPass prog of
    PassOkay _ -> False
    PassFail _ -> True

testPassRmCallsFails :: Program -> Bool
testPassRmCallsFails prog = undefined

testPassesObjAttrAccess :: Program -> Bool
testPassesObjAttrAccess prog = undefined

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

  let pureArgsPairs = map (\(f, p) -> (f, testPureArgCallsFails p)) parsedPairs
  let numPureArgsPairs = length $ filter snd pureArgsPairs
  putStrLn $ "tpod: [pure args] " ++ show numPureArgsPairs ++ "/"
                                  ++ show numParsedPairs

  putStrLn "********************************************"
  return ()

