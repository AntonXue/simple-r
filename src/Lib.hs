module Lib
    ( someFunc
    ) where

import SimpleR.Language
import SimpleR.Interpreter
import SimpleR.Smt
import SimpleR.R

someFunc :: IO ()
someFunc = do
  putStrLn "someFunc"
  return ()

