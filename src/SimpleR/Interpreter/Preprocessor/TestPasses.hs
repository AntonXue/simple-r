module SimpleR.Interpreter.Preprocessor.TestPasses
  ( testPassesOnDir
  ) where

import System.Directory

import SimpleR.Language
import SimpleR.Interpreter.Preprocessor.Passes
import SimpleR.Interpreter.Preprocessor.LinearizationFromFile
import SimpleR.Interpreter.Preprocessor.Loader

testPassesOnDir :: String -> IO ()
testPassesOnDir dir = do
  files <- getDirectoryContents dir
  -- Regular pass, no base
  passes <- mapM (linearizeFileWithPasses dir) files
  let passes2 = zip files passes
  _ <- mapM (\(f, p) -> do
          putStrLn "----------------------"
          case p of
            PassOkay (_, []) -> putStrLn $ "[Error] " ++ show f
            PassOkay _ -> putStrLn $ "[Okay] " ++ f
            PassFail _ -> putStrLn $ "[Error] " ++ show f
        )
        passes2
  return ()

