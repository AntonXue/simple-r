module SimpleR.Interpreter.Preprocessor.Passes.ShowStringPass
  ( showStringPass
  ) where

import Data.List

import SimpleR.Language
import SimpleR.Interpreter.Preprocessor.Passes.PassCommons

showStringPass :: String -> Program -> PassResult Program
showStringPass key prog =
  if key `isInfixOf` show prog then
    PassOkay prog
  else
    PassFail $ ["showStringPass: contains " ++ key]

