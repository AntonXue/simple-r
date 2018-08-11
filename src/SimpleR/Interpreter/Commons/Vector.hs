module SimpleR.Interpreter.Commons.Vector
  ( Type (..)
  , SInt (..)
  , SDouble (..)
  , SComplex (..)
  , SString (..)
  , SBool (..)
  , Vector (..)
  , idFromSString
  , vecFromConst
  , vecLength
  , vecResize
  , vecToType
  ) where

import Data.Maybe
import qualified Data.Complex as C

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

idFromSString :: SString -> Ident
idFromSString NAString = idFromString "NA"
idFromSString (SString str) = idFromString str

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

-----
-- Conversion

maybeRead :: (Read a) => String -> Maybe a
maybeRead str = fmap fst $ listToMaybe $ reads str

-- SInt
sIntFromSInt :: SInt -> SInt
sIntFromSInt sInt = sInt

sIntFromSDouble :: SDouble -> SInt
sIntFromSDouble (NADouble) = NAInt
sIntFromSDouble (SDouble double) = SInt $ round double

sIntFromSComplex :: SComplex -> SInt
sIntFromSComplex (NAComplex) = NAInt
sIntFromSComplex (SComplex complex) = SInt $ round $ C.realPart complex

sIntFromSString :: SString -> SInt
sIntFromSString (NAString) = NAInt
sIntFromSString (SString str) =
  error $ " sIntFromSString: TODO"
--   case maybeRead str of
--     Just int -> SInt int
--     Nothing -> NAInt -- Correct?

sIntFromSBool :: SBool -> SInt
sIntFromSBool (NABool) = NAInt
sIntFromSBool (SBool True) = SInt 1
sIntFromSBool (SBool False) = SInt 0

-- SDouble
sDoubleFromSInt :: SInt -> SDouble
sDoubleFromSInt (NAInt) = NADouble
sDoubleFromSInt (SInt int) = SDouble $ fromIntegral int

sDoubleFromSDouble :: SDouble -> SDouble
sDoubleFromSDouble sDouble = sDouble

sDoubleFromSComplex :: SComplex -> SDouble
sDoubleFromSComplex (NAComplex) = NADouble
sDoubleFromSComplex (SComplex complex) = SDouble $ C.realPart complex

sDoubleFromSString :: SString -> SDouble
sDoubleFromSString (NAString) = NADouble
sDoulbeFromSString (SString str) =
  error $ "sDoubleFromSString: TODO"
--   case maybeRead str of
--     Just double -> SDouble double
--     Nothing -> NADouble

sDoubleFromSBool :: SBool -> SDouble
sDoubleFromSBool (NABool) = NADouble
sDoubleFromSBool (SBool True) = SDouble 1.0
sDoubleFromSBool (SBool False) = SDouble 0.0

-- SComplex
sComplexFromSInt :: SInt -> SComplex
sComplexFromSInt (NAInt) = NAComplex
sComplexFromSInt (SInt int) = SComplex ((fromIntegral int) C.:+ 0.0)

sComplexFromSDouble :: SDouble -> SComplex
sComplexFromSDouble (NADouble) = NAComplex
sComplexFromSDouble (SDouble double) = SComplex (double C.:+ 0.0)

sComplexFromSComplex :: SComplex -> SComplex
sComplexFromSComplex scomplex = scomplex

sComplexFromSString :: SString -> SComplex
sComplexFromSString (NAString) = NAComplex
sComplexFromSString (SString string) =
  error $ "sComplexFromSString: TODO"

sComplexFromSBool :: SBool -> SComplex
sComplexFromSBool (NABool) = NAComplex
sComplexFromSBool (SBool True) = SComplex (1.0 C.:+ 0.0)
sComplexFromSBool (SBool False) = SComplex (0.0 C.:+ 0.0)

-- SString
sStringFromSInt :: SInt -> SString
sStringFromSInt (NAInt) = NAString
sStringFromSInt (SInt int) = SString $ show int

sStringFromSDouble :: SDouble -> SString
sStringFromSDouble (NADouble) = NAString
sStringFromSDouble (SDouble double) = SString $ show double

sStringFromSComplex :: SComplex -> SString
sStringFromSComplex (NAComplex) = NAString
sStringFromSComplex (SComplex complex) = SString $ show complex

sStringFromSString :: SString -> SString
sStringFromSString sstring = sstring

sStringFromSBool :: SBool -> SString
sStringFromSBool (NABool) = NAString
sStringFromSBool (SBool bool) = SString $ show bool

-- SBool
sBoolFromSInt :: SInt -> SBool
sBoolFromSInt (NAInt) = NABool
sBoolFromSInt (SInt int) = SBool $ int /= 0

sBoolFromSDouble :: SDouble -> SBool
sBoolFromSDouble (NADouble) = NABool
sBoolFromSDouble (SDouble double) = SBool $ double /= 0.0

sBoolFromSComplex :: SComplex -> SBool
sBoolFromSComplex (NAComplex) = NABool
sBoolFromSComplex (SComplex complex) = SBool $ complex /= (0.0 C.:+ 0.0)

sBoolFromSString :: SString -> SBool
sBoolFromSString (NAString) = NABool
sBoolFromSString sstring =
  error $ "sBoolFromSString: TODO"

sBoolFromSBool :: SBool -> SBool
sBoolFromSBool sbool = sbool



vecToType :: Vector -> Type -> Vector
vecToType (IntVec xs) IntTy = IntVec $ map sIntFromSInt xs
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

