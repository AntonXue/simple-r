module SimpleR.Interpreter.Preprocessor.RunPasses
  ( runBasePasses
  , runUserPasses
  ) where

import SimpleR.Language
import SimpleR.Interpreter.Commons
import SimpleR.Interpreter.Preprocessor.Passes
-- import SimpleR.Interpreter.Preprocessor.Passes.NamingPass
-- import SimpleR.Interpreter.Preprocessor.Passes.PureArgsPass


-- runBasePasses :: Program -> PassResult
runBasePasses :: Program -> PassResult Program
runBasePasses prog = do
  p1 <- renameBasePrimsPass prog
  return p1

-- runUserPasses :: Program -> PassResult
runUserPasses :: Program -> PassResult Program
runUserPasses prog = do
  p1 <- renameUserPrimsPass prog
  p2 <- pureArgsPass p1
  return p2

