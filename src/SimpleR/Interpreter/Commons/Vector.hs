{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module SimpleR.Interpreter.Commons.Vector
  ( Type (..)
  -- , SInt (..)
  -- , SDouble (..)
  -- , SComplex (..)
  -- , SString (..)
  -- , SBool (..)
  , Atom (..)
  , Vector (..)
  , idFromAtomString
  , vecFromConst
  , vecLength
  , vecResize
  , vecToType
  , vecPairJoinType
  ) where

import Data.Maybe
import qualified Data.Complex as C

import SimpleR.Language
import SimpleR.Smt

data Type = BoolTy | IntTy | DoubleTy | ComplexTy | StringTy | NullTy
  deriving (Eq, Show, Read)

data Atom a where
  Atom :: a -> Atom a
  NAtom :: Atom a

instance (Eq a) => Eq (Atom a) where
  (Atom a) == (Atom b) = a == b
  (NAtom) == (NAtom) = True
  _ == _ = False

instance (Show a) => Show (Atom a) where
  show (Atom a) = "Atom (" ++ show a ++ ")"
  show (NAtom) = "NAtom"

instance (Read a) => Read (Atom a) where
  readsPrec _ = error "readsPrec: Atom read unimplemented"

instance Functor Atom where
  fmap _ NAtom = NAtom
  fmap f (Atom a) = Atom (f a)

instance Applicative Atom where
  pure a = Atom a

  (NAtom) <*> _ = NAtom
  (Atom f) <*> atom = fmap f atom

instance Monad Atom where
  return a = Atom a

  (NAtom) >>= _ = NAtom
  (Atom a) >>= f = f a

data Vector =
    IntVec [Atom Int]
  | DoubleVec [Atom Double]
  | ComplexVec [Atom Complex]
  | BoolVec [Atom Bool]
  | StringVec [Atom String]
  | SymVec SmtIdent Type
  | NilVec
  deriving (Eq, Show, Read)

idFromAtomString :: Atom String -> Ident
idFromAtomString (Atom str) = idFromString str

-- Vector conversion
vecFromConst :: Const -> Vector
vecFromConst (IntConst int) = IntVec [Atom int]
vecFromConst (DoubleConst double) = DoubleVec [Atom double]
vecFromConst (ComplexConst complex) = ComplexVec [Atom complex]
vecFromConst (BoolConst bool) = BoolVec [Atom bool]
vecFromConst (StringConst str) = StringVec [Atom str]
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
vecType (NilVec) = NullTy

vecInts :: Vector -> Maybe [Atom Int]
vecInts (IntVec xs) = Just xs
vecInts _ = Nothing

vecDoubles :: Vector -> Maybe [Atom Double]
vecDoubles (DoubleVec xs) = Just xs
vecDoubles _ = Nothing

vecComplexs :: Vector -> Maybe [Atom Complex]
vecComplexs (ComplexVec xs) = Just xs
vecComplexs _ = Nothing

vecStrings :: Vector -> Maybe [Atom String]
vecStrings (StringVec xs) = Just xs
vecStrings _ = Nothing

vecBools :: Vector -> Maybe [Atom Bool]
vecBools (BoolVec xs) = Just xs
vecBools _ = Nothing

------
-- Resizing

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

-----
-- Conversion

maybeRead :: (Read a) => String -> Maybe a
maybeRead str = fmap fst $ listToMaybe $ reads str

-- Int
intFromInt :: Int -> Int
intFromInt int = int

intFromDouble :: Double -> Int
intFromDouble double = round double

intFromComplex :: Complex -> Int
intFromComplex complex = round $ C.realPart complex

intFromString :: String -> int
intFromString = error $ "intFromString: TODO"

intFromBool :: Bool -> Int
intFromBool bool = if bool then 1 else 0

-- Double
doubleFromInt :: Int -> Double
doubleFromInt int = fromIntegral int

doubleFromDouble :: Double -> Double
doubleFromDouble double = double

doubleFromComplex :: Complex -> Double
doubleFromComplex complex = C.realPart complex

doubleFromString :: String -> Double
doubleFromString = error "doubleFromString: TODO"

doubleFromBool :: Bool -> Double
doubleFromBool bool = if bool then 1.0 else 0.0

-- Complex
complexFromInt :: Int -> Complex
complexFromInt int = (fromIntegral int) C.:+ 0.0

complexFromDouble :: Double -> Complex
complexFromDouble double = double C.:+ 0.0

complexFromComplex :: Complex -> Complex
complexFromComplex complex = complex

complexFromString :: String -> Complex
complexFromString = error "complexFromString: TODO"

complexFromBool :: Bool -> Complex
complexFromBool bool = if bool then 1.0 C.:+ 0.0 else 0.0 C.:+ 0.0

-- String
stringFromInt :: Int -> String
stringFromInt int = show int

stringFromDouble :: Double -> String
stringFromDouble double = show double

stringFromComplex :: Complex -> String
stringFromComplex complex = show complex

stringFromString :: String -> String
stringFromString string = string

stringFromBool :: Bool -> String
stringFromBool bool = show bool

-- Bool
boolFromInt :: Int -> Bool
boolFromInt int = int /= 0

boolFromDouble :: Double -> Bool
boolFromDouble double = double /= 0.0

boolFromComplex :: Complex -> Bool
boolFromComplex complex = complex /= (0.0 C.:+ 0.0)

boolFromString :: String -> Bool
boolFromString = error $ "boolFromString: TODO"

boolFromBool :: Bool -> Bool
boolFromBool bool = bool

vecToType :: Vector -> Type -> Vector
vecToType (IntVec xs) IntTy = IntVec $ (map . fmap) intFromInt xs
vecToType (IntVec xs) DoubleTy = DoubleVec $ (map . fmap) doubleFromInt xs
vecToType (IntVec xs) ComplexTy = ComplexVec $ (map . fmap) complexFromInt xs
vecToType (IntVec xs) StringTy = StringVec $ (map . fmap) stringFromInt xs
vecToType (IntVec xs) BoolTy = BoolVec $ (map . fmap) boolFromInt xs

vecToType (DoubleVec xs) IntTy = IntVec $ (map . fmap) intFromDouble xs
vecToType (DoubleVec xs) DoubleTy =
  DoubleVec $ (map . fmap) doubleFromDouble xs
vecToType (DoubleVec xs) ComplexTy =
  ComplexVec $ (map . fmap) complexFromDouble xs
vecToType (DoubleVec xs) StringTy =
  StringVec $ (map . fmap) stringFromDouble xs
vecToType (DoubleVec xs) BoolTy = BoolVec $ (map . fmap) boolFromDouble xs

vecToType (ComplexVec xs) IntTy = IntVec $ (map . fmap) intFromComplex xs
vecToType (ComplexVec xs) DoubleTy =
  DoubleVec $ (map . fmap) doubleFromComplex xs
vecToType (ComplexVec xs) ComplexTy =
  ComplexVec $ (map . fmap) complexFromComplex xs
vecToType (ComplexVec xs) StringTy =
  StringVec $ (map . fmap) stringFromComplex xs
vecToType (ComplexVec xs) BoolTy = BoolVec $ (map . fmap) boolFromComplex xs

vecToType (StringVec xs) IntTy = IntVec $ (map . fmap) intFromString xs
vecToType (StringVec xs) DoubleTy =
  DoubleVec $ (map . fmap) doubleFromString xs
vecToType (StringVec xs) ComplexTy =
  ComplexVec $ (map . fmap) complexFromString xs
vecToType (StringVec xs) StringTy =
  StringVec $ (map . fmap) stringFromString xs
vecToType (StringVec xs) BoolTy = BoolVec $ (map . fmap) boolFromString xs

vecToType (BoolVec xs) IntTy = IntVec $ (map . fmap) intFromBool xs
vecToType (BoolVec xs) DoubleTy = DoubleVec $ (map . fmap) doubleFromBool xs
vecToType (BoolVec xs) ComplexTy =
  ComplexVec $ (map . fmap) complexFromBool xs
vecToType (BoolVec xs) StringTy = StringVec $ (map . fmap) stringFromBool xs
vecToType (BoolVec xs) BoolTy = BoolVec $ (map . fmap) boolFromBool xs

vecToType (SymVec sym _) ty = SymVec sym ty
vecToType (NilVec) _ = NilVec

-- Type lattice
--  BoolTy <= IntTy <= Double <= Complex <= String <= Null
instance Ord Type where
  BoolTy `compare` BoolTy = EQ
  BoolTy `compare` _ = LT

  IntTy `compare` IntTy = EQ
  IntTy `compare` _ = LT

  DoubleTy `compare` DoubleTy = EQ
  DoubleTy `compare` _ = LT

  ComplexTy `compare` ComplexTy = EQ
  ComplexTy `compare` _ = LT

  StringTy `compare` StringTy = EQ
  StringTy `compare` _ = LT

  NullTy `compare` NullTy = EQ
  NullTy `compare` _ = GT

joinType :: Type -> Type -> Type
joinType ty1 ty2 = if ty1 < ty2 then ty2 else ty1

vecPairJoinType :: Vector -> Vector -> (Vector, Vector)
vecPairJoinType vec1 vec2 =
  let joinTy = joinType (vecType vec1) (vecType vec2) in
    (vecToType vec1 joinTy, vecToType vec2 joinTy)

