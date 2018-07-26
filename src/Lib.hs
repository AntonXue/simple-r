module Lib
    ( someFunc
    ) where

import SimpleR.Language
import SimpleR.Interpreter
import SimpleR.Smt

someFunc :: IO ()
someFunc = putStrLn "someFunc"
