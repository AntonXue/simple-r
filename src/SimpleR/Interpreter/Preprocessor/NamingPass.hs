module SimpleR.Interpreter.Preprocessor.NamingPass
  ( renameBasePrims
  , renameUserPrims
  ) where

import SimpleR.Language
import SimpleR.R
import SimpleR.Interpreter.Natives


-- Rename all the primitive functions that appear in the base program.
renameBasePrims :: Program -> Program
renameBasePrims baseProg =
  let names = map fst primNameDataPairs in
  let pairs = map (\n -> (idFromString n, idPrimFromString n)) names in
    foldl (\p (o, n) -> rename o n p) baseProg pairs

renameUserPrims :: Program -> Program
renameUserPrims userProg =
  let names = map fst primNameDataPairs in
  let pairs = map (\n -> (idFromString n, idPrimFromString n)) names in
    foldl (\p (o, n) -> rename o n p) userProg pairs


