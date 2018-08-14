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

applyBinOp :: VecBinOp -> MemRef -> MemRef -> State ->
  Maybe (Either Vector [SmtCmd], State)
applyBinOp op vecMem1 vecMem2 state
  | Just (DataObj (VecVal vec1) attrs1) <- heapLookup vecMem1 $ stHeap state
  , Just (DataObj (VecVal vec2) attrs2) <- heapLookup vecMem2 $ stHeap state =
      undefined

  | otherwise = Nothing


