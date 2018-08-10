{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module SimpleR.Interpreter.Commons.Vector where

import SimpleR.Language
import SimpleR.Smt

data Type = IntTy | DoubleTy | ComplexTy | BoolTy | StringTy
  deriving (Eq, Show, Read)

data Vector =
    IntVec [Int]
  | DoubleVec [Double]
  | ComplexVec [Complex]
  | BoolVec [Bool]
  | StringVec [String]
  | SymVec SmtIdent Type
  deriving (Eq, Show, Read)

class VecElem a



data Vector2 a where
  IntVec2 :: [Int] -> Vector2 Int
  DoubleVec2 :: [Double] -> Vector2 Double
  ComplexVec2 :: [Complex] -> Vector2 Complex
  BoolVec2 :: [Bool] -> Vector2 Bool
  StringVec2 :: [String] -> Vector2 String
  SymVec2 :: (VecElem a) => SmtIdent -> a -> Vector2 a


-- Vector conversion
vecFromConst :: Const -> Vector
vecFromConst (IntConst int) = IntVec [int]
vecFromConst (DoubleConst double) = DoubleVec [double]
vecFromConst (ComplexConst complex) = ComplexVec [complex]
vecFromConst (BoolConst bool) = BoolVec [bool]
vecFromConst (StringConst str) = StringVec [str]

vecLength :: Vector -> Int
vecLength (IntVec xs) = length xs
vecLength (DoubleVec xs) = length xs
vecLength (ComplexVec xs) = length xs
vecLength (BoolVec xs) = length xs
vecLength (StringVec xs) = length xs
vecLength (SymVec _ _ ) = error $ "vecLength: called with SymVec"

vecType :: Vector -> Type
vecType (IntVec _) = IntTy
vecType (DoubleVec _) = DoubleTy
vecType (ComplexVec _) = ComplexTy
vecType (BoolVec _) = BoolTy
vecType (StringVec _) = StringTy
vecType (SymVec _ ty) = ty


vecResize :: Vector -> Int -> Vector
vecResize = undefined

