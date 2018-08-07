module SimpleR.Interpreter.Preprocessor.Passes.NamingPass
  ( renameBasePrimsPass
  , renameUserPrimsPass
  ) where

import SimpleR.Language
import SimpleR.R
import SimpleR.Interpreter.Natives
import SimpleR.Interpreter.Preprocessor.Passes.PassCommons


-- Rename all the primitive functions that appear in the base program.
renameBasePrimsPass :: Program -> PassResult Program
renameBasePrimsPass baseProg =
  let names = map fst primNameDataPairs in
  let pairs = map (\n -> (idFromString n, idPrimFromString n)) names in
    PassOkay $ foldl (\p (o, n) -> rename o n p) baseProg pairs

-- Should we want to prevent the user from renaming primitive functions?
renameUserPrimsPass :: Program -> PassResult Program
renameUserPrimsPass userProg =
  let names = map fst primNameDataPairs in
  let pairs = map (\n -> (idFromString n, idPrimFromString n)) names in
    PassOkay $ foldl (\p (o, n) -> rename o n p) userProg pairs


