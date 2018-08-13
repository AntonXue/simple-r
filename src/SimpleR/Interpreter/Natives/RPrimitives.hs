module SimpleR.Interpreter.Natives.RPrimitives
  ( RPrim (..)
  , rPrimToString
  , idFromRPrim
  , idStupidFromRPrim
  , idPrimFromString
  , primAll
  , primIds
  , primIdSet
  , primNameDataPairs
  , primInjectionPairs
  ) where

import Data.Maybe
import qualified Data.Set as S

import SimpleR.Language
import SimpleR.Interpreter.Commons

data RPrim =
    RPRIM

  | RPrimAnyNa

  | RPrimAsCharacter
  | RPrimAsComplex
  | RPrimAsDouble
  | RPrimAsEnvironment
  | RPrimAsInteger
  | RPrimAsLogical
  | RPrimAsCall
  | RPrimAsNumeric
  | RPrimAsRaw

  | RPrimC
  | RPrimDim
  | RPrimDimArrow
  | RPrimDimNames
  | RPrimDimNamesAssign

  | RPrimIsArray
  | RPrimIsFinite
  | RPrimIsInfinite
  | RPrimIsMatrix
  | RPrimIsNa
  | RPrimIsNan
  | RPrimIsNumeric

  | RPrimLength
  | RPrimLengthAssign
  | RPrimLevels
  | RPrimLevelsAssign
  | RPrimNames
  | RPrimNamesAssign
  | RPrimRep
  | RPrimSeqInt
  | RPrimXtfrm

  | RPrimAbs
  | RPrimSign
  | RPrimSqrt
  | RPrimFloor
  | RPrimCeiling
  | RPrimExp
  | RPrimExpm1
  | RPrimLog1p
  | RPrimLog10
  | RPrimLog2
  | RPrimCos
  | RPrimSin
  | RPrimTan
  | RPrimAcos
  | RPrimAsin
  | RPrimAtan
  | RPrimCosh
  | RPrimSinh
  | RPrimTanh
  | RPrimAcosh
  | RPrimAsinh
  | RPrimAtanh
  | RPrimCosPi
  | RPrimSinPi
  | RPrimTanPi

  | RPrimGamma
  | RPrimLGamma
  | RPrimDiGamma
  | RPrimTriGamma
  | RPrimCumSum
  | RPrimCumProd
  | RPrimCumMax
  | RPrimCumMin

  | RPrimPlus
  | RPrimMinus
  | RPrimMult
  | RPrimDiv
  | RPrimPow
  | RPrimMod
  | RPrimIntDiv

  | RPrimAnd
  | RPrimOr
  | RPrimEq
  | RPrimNeq
  | RPrimLt
  | RPrimLe
  | RPrimGe
  | RPrimGt

  | RPrimAll
  | RPrimAny
  | RPrimSum
  | RPrimProd
  | RPrimMax
  | RPrimMin
  | RPrimRange

  | RPrimArg
  | RPrimConj
  | RPrimIm
  | RPrimModulus
  | RPrimReal

  -- Extras that we add
  | RPrimNot
  | RPrimAssign
  | RPrimSuperAssign
  | RPrimColon
  | RPrimForm
  | RPrimHelp
  | RPrimVecProj
  | RPrimVecSub

  | RPrimGetPackage
  | RPrimGetPackageInt
  deriving (Ord, Eq, Show, Read)

-- Utility functions for working with primitives

idPrimFromString :: String -> Ident
idPrimFromString str =
  Ident { idName = str, idPkg = Just "prim", idAnnot = Nothing }

rPrimToString :: RPrim -> String
rPrimToString prim =
  case map fst $ filter ((== prim) . snd) primNameDataPairs of
    [] -> error $ "rPrimToString: failed to account for " ++ show prim
    (str : []) -> str
    (_ : _) -> error $ "rPrimToString: too many matches for " ++ show prim

idFromRPrim :: RPrim -> Ident
idFromRPrim prim =  idPrimFromString $ rPrimToString prim

idStupidFromRPrim :: RPrim -> Ident
idStupidFromRPrim prim = idFromString $ rPrimToString prim

idsFromParams :: [Param] -> [Ident]
idsFromParams [] = []
idsFromParams ((Param id) : params) = id : idsFromParams params
idsFromParams ((Default id _) : params) = id : idsFromParams params
idsFromParams (VarParam : params) = idVariadic : idsFromParams params

-- The important stuff for making the data tables

params0 :: [Param]
params0 = []

params1 :: [Param]
params1 = map (Param . idFromString) ["a"]

params2 :: [Param]
params2 = map (Param . idFromString) ["a", "b"]

params3 :: [Param]
params3 = map (Param . idFromString) ["a", "b", "c"]


-- https://github.com/wch/r-source/blob/a6d3738aeb73c748f27a94a2c97bb5750c6e01bf/src/library/base/R/zzz.R#L150-#L210
primAll :: [(String, RPrim, [Param])]
primAll =
  [ ("RPRIM", RPRIM, [])

  -- Conversion
  , ("as.character",    RPrimAsCharacter,   [])
  , ("as.complex",      RPrimAsComplex,     [])
  , ("as.double",       RPrimAsDouble,      [])
  , ("as.environment",  RPrimAsEnvironment, [])
  , ("as.integer",      RPrimAsInteger,     [])
  , ("as.logical",      RPrimAsLogical,     [])
  , ("as.call",         RPrimAsCall,        [])
  , ("as.numeric",      RPrimAsNumeric,     [])
  , ("as.raw",          RPrimAsRaw,         [])

  -- Vectors
  , ("c",           RPrimC,              [])
  , (":",           RPrimColon,          [])
  , ("[[",  RPrimVecProj,         [])
  , ("[",   RPrimVecSub,          [])
  , ("dim",         RPrimDim,            [])
  , ("dim<-",       RPrimDimArrow,       [])
  , ("dimnames",    RPrimDimNames,       [])
  , ("dimnames<-",  RPrimDimNamesAssign, [])
  , ("length",      RPrimLength,        [])
  , ("length<-",    RPrimLengthAssign,  [])
  , ("levels",      RPrimLevels,        [])
  , ("levels<-",    RPrimLevelsAssign,  [])
  , ("names",       RPrimNames,         [])
  , ("names<-",     RPrimNamesAssign,   [])
  , ("rep",         RPrimRep,     [])
  , ("seq.int",     RPrimSeqInt,  [])
  , ("xtfrm",       RPrimXtfrm,   [])
  , ("anyNA",           RPrimAnyNa,         [])

  -- Is operations
  , ("is.array",    RPrimIsArray,     [])
  , ("is.finite",   RPrimIsFinite,    [])
  , ("is.infinite", RPrimIsInfinite,  [])
  , ("is.matrix",   RPrimIsMatrix,    [])
  , ("is.na",       RPrimIsNa,        [])
  , ("is.nan",      RPrimIsNan,       [])
  , ("is.numeric",  RPrimIsNumeric,   [])

  -- Numerical
  , ("abs",     RPrimAbs,     [])
  , ("sign",    RPrimSign,    [])
  , ("sqrt",    RPrimSqrt,    [])
  , ("floor",   RPrimFloor,   [])
  , ("ceiling", RPrimCeiling, [])
  , ("exp",     RPrimExp,     [])
  , ("expm1",   RPrimExpm1,   [])
  , ("log1p",   RPrimLog1p,   [])
  , ("log10",   RPrimLog10,   [])
  , ("log2",    RPrimLog2,    [])

  , ("cos",   RPrimCos,   [])
  , ("sin",   RPrimSin,   [])
  , ("tan",   RPrimTan,   [])
  , ("acos",  RPrimAcos,  [])
  , ("asin",  RPrimAsin,  [])
  , ("atan",  RPrimAtan,  [])
  , ("cosh",  RPrimCosh,  [])
  , ("sinh",  RPrimSinh,  [])
  , ("tanh",  RPrimTanh,  [])
  , ("acosh", RPrimAcosh, [])
  , ("asinh", RPrimAsinh, [])
  , ("atanh", RPrimAtanh, [])
  , ("cospi", RPrimCosPi, [])
  , ("sinpi", RPrimSinPi, [])
  , ("tanpi", RPrimTanPi, [])

  , ("gamma",     RPrimGamma,     [])
  , ("lgamma",    RPrimLGamma,    [])
  , ("digamma",   RPrimDiGamma,   [])
  , ("trigamma",  RPrimTriGamma,  [])
  , ("cumsum",    RPrimCumSum,    [])
  , ("cumprod",   RPrimCumProd,   [])
  , ("cummax",    RPrimCumMax,    [])
  , ("cummin",    RPrimCumMin,    [])

  -- More numerical
  , ("+",   RPrimPlus,    [])
  , ("-",   RPrimMinus,   [])
  , ("*",   RPrimMult,    [])
  , ("/",   RPrimDiv,     [])
  , ("^",   RPrimPow,     [])
  , ("%%",  RPrimMod,     [])
  , ("%/%", RPrimIntDiv,  [])
  , ("&",   RPrimAnd,     [])
  , ("|",   RPrimOr,      [])
  , ("==",  RPrimEq,      [])
  , ("!=",  RPrimNeq,     [])
  , ("<",   RPrimLt,      [])
  , ("<=",  RPrimLe,      [])
  , (">=",  RPrimGe,      [])
  , (">",   RPrimGt,      [])
  , ("!",   RPrimNot,     [])

  , ("all", RPrimAll,     [])
  , ("any", RPrimAny,     [])
  , ("sum", RPrimSum,     [])
  , ("prod", RPrimProd,   [])
  , ("max", RPrimMax,     [])
  , ("min", RPrimMin,     [])
  , ("range", RPrimRange, [])

  -- Complex numbers
  , ("Arg",   RPrimArg,     [])
  , ("Conj",  RPrimConj,    [])
  , ("Im",    RPrimIm,      [])
  , ("Mod",   RPrimModulus, [])
  , ("Re",    RPrimReal,    [])

  -- Assignment
  , ("<-",  RPrimAssign,          [])
  , ("<<-", RPrimSuperAssign,     [])

  -- Lookups
  , ("::",  RPrimGetPackage,      [])
  , (":::", RPrimGetPackageInt,   [])

  -- Weird things
  , ("~",   RPrimForm,            [])
  , ("?",   RPrimHelp,            [])
  ]

primIds :: [Ident]
primIds = map (\(n, _, _) -> idPrimFromString n) primAll

primIdSet :: S.Set Ident
primIdSet = S.fromList primIds

primNameDataPairs :: [(String, RPrim)]
primNameDataPairs = map (\(n, d, _) -> (n, d)) primAll

primInjectionPairs :: [(Ident, ([Param], Expr))]
primInjectionPairs =
  map (\(name, _, params) ->
        let primId = idPrimFromString name in
        let paramsId = idsFromParams params in
          (primId,  (params, NativeLamApp primId paramsId)))
      primAll

