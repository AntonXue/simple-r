module SimpleR.Interpreter.Preprocessor.RunPasses
  ( runBasePasses
  , runUserPasses
  ) where

import SimpleR.Language
import SimpleR.Interpreter.Commons
import SimpleR.Interpreter.Preprocessor.Passes.NamingPass
import SimpleR.Interpreter.Preprocessor.Passes.PureArgsPass

type PassResult = Either Program [String]

composePasses ::
  (Program -> PassResult) -> (Program -> PassResult) -> (Program -> PassResult)
composePasses pass1 pass2 prog =
  case pass1 prog of
    Right msgs -> Right msgs
    Left prog2 -> pass2 prog2

-- runBasePasses :: Program -> PassResult
runBasePasses :: Program -> Either Program [String]
runBasePasses prog = undefined

-- runUserPasses :: Program -> PassResult
runUserPasses :: Program -> Either Program [String]
runUserPasses prog = undefined

