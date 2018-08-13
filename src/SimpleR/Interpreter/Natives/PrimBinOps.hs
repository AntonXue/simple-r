module SimpleR.Interpreter.Natives.PrimBinOps

  where

import SimpleR.Language
import SimpleR.Interpreter.Commons
import SimpleR.Smt

--   -- More Numerical
--   , ("+",   RPrimPlus,    [])
--   , ("-",   RPrimMinus,   [])
--   , ("*",   RPrimMult,    [])
--   , ("/",   RPrimDiv,     [])
--   , ("^",   RPrimPow,     [])
--   , ("%%",  RPrimMod,     [])
--   , ("%/%", RPrimIntDiv,  [])
--   , ("&",   RPrimAnd,     [])
--   , ("|",   RPrimOr,      [])
--   , ("==",  RPrimEq,      [])
--   , ("!=",  RPrimNeq,     [])
--   , ("<",   RPrimLt,      [])
--   , ("<=",  RPrimLe,      [])
--   , (">=",  RPrimGe,      [])
--   , (">",   RPrimGt,      [])
--   , ("!",   RPrimNot,     [])

data VecBinOp = VecBinOp
  { intBinOp :: Int -> Int -> Int
  , doubleBinOp :: Double -> Double -> Double
  , complexBinOp :: Complex -> Complex -> Complex
  , stringBinOp :: String -> String -> String
  , boolBinOp :: Bool -> Bool -> Bool
  , symBinOp :: SmtIdent -> SmtIdent -> Type -> State -> ([SmtExpr], State)
  }

applyBinOp :: VecBinOp -> Vector ->  Vector -> State ->
  (Either Vector [SmtExpr], State)
applyBinOp op rawVec1 rawVec2 state =
  let (vec1, vec2) = vecPairJoinType rawVec1 rawVec2 in
    case (vec1, vec2) of
      (SymVec sym1 _, _) -> undefined
      (_, SymVec sym2 _) -> undefined

      (IntVec _, _) -> undefined



