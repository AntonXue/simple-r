{-# LANGUAGE GADTs #-}

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
  -- , idFromSString
  -- , vecFromConst
  -- , vecLength
  -- , vecResize
  -- , vecToType
  ) where

import Data.Maybe
import qualified Data.Complex as C

import SimpleR.Language
import SimpleR.Smt

data Type = IntTy | DoubleTy | ComplexTy | BoolTy | StringTy
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

-- SBool
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
{-
vecToType (IntVec xs) DoubleTy = DoubleVec $ map sDoubleFromSInt xs
vecToType (IntVec xs) ComplexTy = ComplexVec $ map sComplexFromSInt xs
vecToType (IntVec xs) StringTy = StringVec $ map sStringFromSInt xs
vecToType (IntVec xs) BoolTy = BoolVec $ map sBoolFromSInt xs

vecToType (DoubleVec xs) IntTy = IntVec $ map sIntFromSDouble xs
vecToType (DoubleVec xs) DoubleTy = DoubleVec $ map sDoubleFromSDouble xs
vecToType (DoubleVec xs) ComplexTy = ComplexVec $ map sComplexFromSDouble xs
vecToType (DoubleVec xs) StringTy = StringVec $ map sStringFromSDouble xs
vecToType (DoubleVec xs) BoolTy = BoolVec $ map sBoolFromSDouble xs

vecToType (ComplexVec xs) IntTy = IntVec $ map sIntFromSComplex xs
vecToType (ComplexVec xs) DoubleTy = DoubleVec $ map sDoubleFromSComplex xs
vecToType (ComplexVec xs) ComplexTy = ComplexVec $ map sComplexFromSComplex xs
vecToType (ComplexVec xs) StringTy = StringVec $ map sStringFromSComplex xs
vecToType (ComplexVec xs) BoolTy = BoolVec $ map sBoolFromSComplex xs

vecToType (StringVec xs) IntTy = IntVec $ map sIntFromSString xs
vecToType (StringVec xs) DoubleTy = DoubleVec $ map sDoubleFromSString xs
vecToType (StringVec xs) ComplexTy = ComplexVec $ map sComplexFromSString xs
vecToType (StringVec xs) StringTy = StringVec $ map sStringFromSString xs
vecToType (StringVec xs) BoolTy = BoolVec $ map sBoolFromSString xs

vecToType (BoolVec xs) IntTy = IntVec $ map sIntFromSBool xs
vecToType (BoolVec xs) DoubleTy = DoubleVec $ map sDoubleFromSBool xs
vecToType (BoolVec xs) ComplexTy = ComplexVec $ map sComplexFromSBool xs
vecToType (BoolVec xs) StringTy = StringVec $ map sStringFromSBool xs
vecToType (BoolVec xs) BoolTy = BoolVec $ map sBoolFromSBool xs

vecToType (SymVec sym _) ty = SymVec sym ty
vecToType (NilVec) _ = NilVec
-}

-- Operations

{-
mapSInt :: (Int -> Int) -> [SInt] -> [SInt]
mapSInt _ [] = []
mapSInt f (NAInt : xs) = NAInt : mapSInt f xs
mapSInt f ((SInt x) : xs) = (SInt $ f x) : mapSInt f xs

mapSDouble :: (Double -> Double) -> [SDouble] -> [SDouble]
mapSDouble _ [] = []
mapSDouble f (NADouble : xs) = NADouble : mapSDouble f xs
mapSDouble f ((SDouble x) : xs) = (SDouble $ f x) : mapSDouble f xs

mapSComplex :: (Complex -> Complex) -> [SComplex] -> [SComplex]
mapSComplex _ [] = []
mapSComplex f (NAComplex : xs) = NAComplex : mapSComplex f xs
mapSComplex f ((SComplex x) : xs) = (SComplex $ f x) : mapSComplex f xs

mapSString :: (String -> String) -> [SString] -> [SString]
mapSString _ [] = []
mapSString f (NAString : xs) = NAString : mapSString f xs
mapSString f ((SString x) : xs) = (SString $ f x) : mapSString f xs

mapSBool :: (Bool -> Bool) -> [SBool] -> [SBool]
mapSBool _ [] = []
mapSBool f (NABool : xs) = NABool : mapSBool f xs
mapSBool f ((SBool x) : xs) = (SBool $ f x) : mapSBool f xs

data VectorUnOp = VectorUnOp
  { intUnOp :: Int -> Int
  , doubleUnOp :: Double -> Double
  , complexUnOp :: Complex -> Complex
  , stringUnOp :: String -> String
  , boolUnOp :: Bool -> Bool
  }

vecUnOpMk ::
  (Int -> Int) ->
  (Double -> Double) ->
  (Complex -> Complex) ->
  (String -> String) ->
  (Bool -> Bool) -> VectorUnOp
vecUnOpMk funInt funDouble funComplex funString funBool =
  VectorUnOp { intUnOp = funInt
             , doubleUnOp = funDouble
             , complexUnOp = funComplex
             , stringUnOp = funString
             , boolUnOp = funBool }

data VectorBinOp = VectorBinOp
  { intBinOp :: Int -> Int -> Int
  , doubleBinOp :: Double -> Double -> Double
  , complexBinOp :: Complex -> Complex -> Complex
  , stringBinOp :: String -> String -> String
  , boolBinOp :: Bool -> Bool -> Bool
  }

vecBinOpMk ::
  (Int -> Int -> Int) ->
  (Double -> Double -> Double) ->
  (Complex -> Complex -> Complex) ->
  (String -> String -> String) ->
  (Bool -> Bool -> Bool) -> VectorBinOp
vecBinOpMk funInt funDouble funComplex funString funBool =
  VectorBinOp { intBinOp = funInt
             , doubleBinOp = funDouble
             , complexBinOp = funComplex
             , stringBinOp = funString
             , boolBinOp = funBool }

applyVecUnOp :: VectorUnOp -> Vector -> Vector
applyVecUnOp un (IntVec xs) =
  let f = intUnOp un in
    IntVec $ mapSInt f xs
applyVecUnOp un (DoubleVec xs) =
  let f = doubleUnOp un in
    DoubleVec $ mapSDouble f xs
applyVecUnOp un (ComplexVec xs) =
  let f = complexUnOp un in
    ComplexVec $ mapSComplex f xs
applyVecUnOp un (StringVec xs) =
  let f = stringUnOp un in
    StringVec $ mapSString f xs
applyVecUnOp un (BoolVec xs) =
  let f = boolUnOp un in
    BoolVec $ mapSBool f xs
applyVecUnOp _ (NilVec) = NilVec
applyVecUnOp _ (SymVec _ _) = error $ "applyVecUnOp: used on SymVec!"

-- Assumed vectors to be coerced and zipped
applyVecBinOp :: VectorBinOp -> Vector -> Vector -> Vector
applyVecBinOp bin (IntVec xs) (IntVec ys) =
  let f = intBinOp bin in
    IntVec $ mapSInt (\(x, y) -> f x y) $ zip xs ys
applyVecBinOp bin (DoubleVec xs) (DoubleVec ys) =
  let f = doubleBinOp bin in
    DoubleVec $ mapSDouble (\(x, y) -> f x y) $ zip xs ys
applyVecBinOp bin (ComplexVec xs) (ComplexVec ys) =
  let f = complexBinOp bin in
    ComplexVec $ mapSComplex (\(x, y) -> f x y) $ zip xs ys
applyVecBinOp bin (StringVec xs) (StringVec ys) =
  let f = stringBinOp bin in
    StringVec $ mapSString (\(x, y) -> f x y) $ zip xs ys
applyVecBinOp bin (BoolVec xs) (BoolVec ys) =
  let f = boolBinOp bin in
    BoolVec $ mapSBool (\(x, y) -> f x y) $ zip xs ys
applyVecBinOp _ (NilVec) _ = NilVec
applyVecBinOp _ _ (NilVec) = NilVec
applyVecBinOp _ (SymVec _ _) _ = error $ "applyVecBinOp: used on SymVec!"
applyVecBinOp _ _ (SymVec _ _) = error $ "applyVecBinOp: used on SymVec!"
applyVecBinOp _ vec1 vec2 =
  error $ "applyVecBinOp: fall through " ++ show (vec1, vec2)



-}

