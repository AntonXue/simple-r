module SimpleR.Interpreter.Natives.RPrimitives
  ( RPrim (..)
  , rPrimToString
  , idFromRPrim
  , idStupidFromRPrim
  , idPrimFromString
  , primAll
  , primIds
  , primNameDataPairs
  , primInjectionPairs
  ) where

import Data.Maybe

import SimpleR.Language
import SimpleR.Interpreter.Commons

data RPrim =
    RPrimAnyNa
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
  [ ("anyNA",           RPrimAnyNa,         [])
  , ("as.character",    RPrimAsCharacter,   [])
  , ("as.complex",      RPrimAsComplex,     [])
  , ("as.double",       RPrimAsDouble,      [])
  , ("as.environment",  RPrimAsEnvironment, [])
  , ("as.integer",      RPrimAsInteger,     [])
  , ("as.logical",      RPrimAsLogical,     [])
  , ("as.call",         RPrimAsCall,        [])
  , ("as.numeric",      RPrimAsNumeric,     [])
  , ("as.raw",          RPrimAsRaw,         [])

  , ("c",           RPrimC,              [])
  , ("dim",         RPrimDim,            [])
  , ("dim<-",       RPrimDimArrow,       [])
  , ("dimnames",    RPrimDimNames,       [])
  , ("dimnames<-",  RPrimDimNamesAssign, [])

  , ("is.array",    RPrimIsArray,     [])
  , ("is.finite",   RPrimIsFinite,    [])
  , ("is.infinite", RPrimIsInfinite,  [])
  , ("is.matrix",   RPrimIsMatrix,    [])
  , ("is.na",       RPrimIsNa,        [])
  , ("is.nan",      RPrimIsNan,       [])
  , ("is.numeric",  RPrimIsNumeric,   [])

  , ("length",    RPrimLength,        [])
  , ("length<-",  RPrimLengthAssign,  [])
  , ("levels<-",  RPrimLevelsAssign,  [])
  , ("names",     RPrimNames,         [])
  , ("names<-",   RPrimNamesAssign,   [])

  , ("rep",     RPrimRep,     [])
  , ("seq.int", RPrimSeqInt,  [])
  , ("xtfrm",   RPrimXtfrm,   [])

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

  , ("all", RPrimAll,     [])
  , ("any", RPrimAny,     [])
  , ("sum", RPrimSum,     [])
  , ("prod", RPrimProd,   [])
  , ("max", RPrimMax,     [])
  , ("min", RPrimMin,     [])
  , ("range", RPrimRange, [])

  , ("Arg",   RPrimArg,     [])
  , ("Conj",  RPrimConj,    [])
  , ("Im",    RPrimIm,      [])
  , ("Mod",   RPrimModulus, [])
  , ("Re",    RPrimReal,    [])

  -- Things that were not explicitly included but nice for smooth translation.
  , ("!",   RPrimNot,         [])
  , (":",   RPrimColon,       [])
  , ("<-",  RPrimAssign,      [])
  , ("<<-", RPrimSuperAssign, [])
  , ("~",   RPrimForm,        [])
  , ("?",   RPrimHelp,        [])
  , ("[[",  RPrimVecProj,     [])
  , ("[",   RPrimVecSub,      [])
  ]

primIds :: [Ident]
primIds = map (\(n, _, _) -> idPrimFromString n) primAll

primNameDataPairs :: [(String, RPrim)]
primNameDataPairs = map (\(n, d, _) -> (n, d)) primAll

primInjectionPairs :: [(Ident, ([Param], Expr))]
primInjectionPairs =
  map (\(name, _, params) ->
        let primId = idPrimFromString name in
        let paramsId = idsFromParams params in
          (primId,  (params, NativeLamApp primId paramsId)))
      primAll

