module Lib
    ( module SimpleR.Language
    , module SimpleR.Interpreter
    , module SimpleR.Smt
    , module SimpleR.R
    ) where

import SimpleR.Language
import SimpleR.Interpreter
import SimpleR.Smt
import SimpleR.R

someFunc :: IO ()
someFunc = do
  putStrLn "someFunc"
  return ()

