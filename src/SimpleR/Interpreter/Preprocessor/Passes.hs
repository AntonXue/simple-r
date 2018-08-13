module SimpleR.Interpreter.Preprocessor.Passes
  ( module SimpleR.Interpreter.Preprocessor.Passes.PassCommons
  , module SimpleR.Interpreter.Preprocessor.Passes.NamingPass
  , module SimpleR.Interpreter.Preprocessor.Passes.NonPrimAssignPass
  , module SimpleR.Interpreter.Preprocessor.Passes.PureArgsPass
  , runBasePasses
  , runUserPasses
  ) where

import SimpleR.Language

import SimpleR.Interpreter.Preprocessor.Passes.PassCommons
import SimpleR.Interpreter.Preprocessor.Passes.NamingPass
import SimpleR.Interpreter.Preprocessor.Passes.NonPrimAssignPass
import SimpleR.Interpreter.Preprocessor.Passes.PureArgsPass

progFromPairs :: [(String, Expr)] -> ([String], Program)
progFromPairs pairs =
  let (files, exprs) = unzip pairs in
    (files, Program exprs)

filesProgToPairs :: [String] -> Program -> [(String, Expr)]
filesProgToPairs files (Program exprs) =
  if length files == length exprs then
    zip files exprs
  else
    error $ "filesProgToPairs: files and exprs not the same length"

-- runBasePasses :: Program -> PassResult
runBasePasses :: [(String, Expr)] -> PassResult [(String, Expr)]
runBasePasses pairs = do
  -- Renaming primitives pass
  let (files1, prog1) = progFromPairs pairs
  p1 <- renameBasePrimsPass prog1
  let pairs1 = filesProgToPairs files1 p1
  
  -- Done!
  return $ pairs1

-- 
runUserPasses :: [(String, Expr)] -> PassResult [(String, Expr)]
runUserPasses pairs = do
  -- Renaming primitives pass
  let (files1, prog1) = progFromPairs pairs
  p1 <- renameBasePrimsPass prog1

  let pairsFinal = filesProgToPairs files1 p1

  -- Done!
  return $ pairsFinal

