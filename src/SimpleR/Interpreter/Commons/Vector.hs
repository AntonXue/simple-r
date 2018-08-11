{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module SimpleR.Interpreter.Commons.Vector where

import SimpleR.Language
import SimpleR.Smt


data Type = IntTy | DoubleTy | ComplexTy | BoolTy | StringTy
  deriving (Eq, Show, Read)

data SInt =
    SInt Int
  | NAInt
  deriving (Ord, Eq, Show, Read)

data SDouble =
    SDouble Double
  | NADouble
  deriving (Ord, Eq, Show, Read)

data SComplex =
    SComplex Complex
  | NAComplex
  deriving (Eq, Show, Read)

data SString =
    SString String
  | NAString
  deriving (Ord, Eq, Show, Read)

data SBool =
    SBool Bool
  | NABool
  deriving (Ord, Eq, Show, Read)

data Vector =
    IntVec [SInt]
  | DoubleVec [SDouble]
  | ComplexVec [SComplex]
  | BoolVec [SBool]
  | StringVec [SString]
  | SymVec SmtIdent Type
  | NilVec
  deriving (Eq, Show, Read)

-- Vector conversion
vecFromConst :: Const -> Vector
vecFromConst (IntConst int) = IntVec [SInt int]
vecFromConst (DoubleConst double) = DoubleVec [SDouble double]
vecFromConst (ComplexConst complex) = ComplexVec [SComplex complex]
vecFromConst (BoolConst bool) = BoolVec [SBool bool]
vecFromConst (StringConst str) = StringVec [SString str]
vecFroMConst (NaConst) = NilVec

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

snip :: Int -> [a] -> [a]
snip n xs = take n $ concat $ repeat xs

vecResize :: Int -> Vector -> Vector
vecResize n (IntVec (x : xs)) = IntVec $ snip n (x : xs)
vecResize n (DoubleVec (x : xs)) = DoubleVec $ snip n (x : xs)
vecResize n (ComplexVec (x : xs)) = ComplexVec $ snip n (x : xs)
vecResize n (StringVec (x : xs)) = StringVec $ snip n (x : xs)
vecResize n (BoolVec (x : xs)) = BoolVec $ snip n (x : xs)
vecResize _ (IntVec []) = error $ "vecLength: IntVec has length 0"
vecResize _ (DoubleVec []) = error $ "vecLength: DoubleVec has length 0"
vecResize _ (ComplexVec []) = error $ "vecLength: ComplexVec has length 0"
vecResize _ (StringVec []) = error $ "vecLength: StringVec has length 0"
vecResize _ (BoolVec []) = error $ "vecLength: BoolVec has length 0"
vecResize _ (SymVec _ _) = error $ "vecLength: called with SymVec"
vecResize _ (NilVec) = error $ "vecLength: called with NilVec"

idFromSString :: SString -> Ident
idFromSString NAString = idFromString "NA"
idFromSString (SString str) = idFromString str

