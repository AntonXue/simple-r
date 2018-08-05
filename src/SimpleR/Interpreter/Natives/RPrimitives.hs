module SimpleR.Interpreter.Natives.RPrimitives
  ( RPrim (..)
  , rPrimToString
  , idFromRPrim
  , idStupidFromRPrim
  ) where

import SimpleR.Language
import Data.Maybe

mkRPrimId :: String -> Ident
mkRPrimId str =
  Ident { idName = str, idPkg = Just "prim", idAnnot = Nothing }

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


-- https://github.com/wch/r-source/blob/a6d3738aeb73c748f27a94a2c97bb5750c6e01bf/src/library/base/R/zzz.R#L150-#L210
primNames :: [(String, RPrim)]
primNames =
  [ ("anyNA", RPrimAnyNa)
  , ("as.character", RPrimAsCharacter)
  , ("as.complex", RPrimAsComplex)
  , ("as.double", RPrimAsDouble)
  , ("as.environment", RPrimAsEnvironment)
  , ("as.integer", RPrimAsInteger)
  , ("as.logical", RPrimAsLogical)
  , ("as.call", RPrimAsCall)
  , ("as.numeric", RPrimAsNumeric)
  , ("as.raw", RPrimAsRaw)
  , ("c", RPrimC)
  , ("dim", RPrimDim)
  , ("dim<-", RPrimDimArrow)
  , ("dimnames", RPrimDimNames)
  , ("dimnames<-", RPrimDimNamesAssign)
  , ("is.array", RPrimIsArray)
  , ("is.finite", RPrimIsFinite)
  , ("is.infinite", RPrimIsInfinite)
  , ("is.matrix", RPrimIsMatrix)
  , ("is.na", RPrimIsNa)
  , ("is.nan", RPrimIsNan)
  , ("is.numeric", RPrimIsNumeric)
  , ("length", RPrimLength)
  , ("length<-", RPrimLengthAssign)
  , ("levels<-", RPrimLevelsAssign)
  , ("names", RPrimNames)
  , ("names<-", RPrimNamesAssign)
  , ("rep", RPrimRep)
  , ("seq.int", RPrimSeqInt)
  , ("xtfrm", RPrimXtfrm)

  , ("abs", RPrimAbs)
  , ("sign", RPrimSign)
  , ("sqrt", RPrimSqrt)
  , ("floor", RPrimFloor)
  , ("ceiling", RPrimCeiling)
  , ("exp", RPrimExp)
  , ("expm1", RPrimExpm1)
  , ("log1p", RPrimLog1p)
  , ("log10", RPrimLog10)
  , ("log2", RPrimLog2)
  , ("cos", RPrimCos)
  , ("sin", RPrimSin)
  , ("tan", RPrimTan)
  , ("acos", RPrimAcos)
  , ("asin", RPrimAsin)
  , ("atan", RPrimAtan)
  , ("cosh", RPrimCosh)
  , ("sinh", RPrimSinh)
  , ("tanh", RPrimTanh)
  , ("acosh", RPrimAcosh)
  , ("asinh", RPrimAsinh)
  , ("atanh", RPrimAtanh)
  , ("cospi", RPrimCosPi)
  , ("sinpi", RPrimSinPi)
  , ("tanpi", RPrimTanPi)
  , ("gamma", RPrimGamma)
  , ("lgamma", RPrimLGamma)
  , ("digamma", RPrimDiGamma)
  , ("trigamma", RPrimTriGamma)
  , ("cumsum", RPrimCumSum)
  , ("cumprod", RPrimCumProd)
  , ("cummax", RPrimCumMax)
  , ("cummin", RPrimCumMin)

  , ("+", RPrimPlus)
  , ("-", RPrimMinus)
  , ("*", RPrimMult)
  , ("/", RPrimDiv)
  , ("^", RPrimPow)
  , ("%%", RPrimMod)
  , ("%/%", RPrimIntDiv)
  , ("&", RPrimAnd)
  , ("|", RPrimOr)
  , ("==", RPrimEq)
  , ("!=", RPrimNeq)
  , ("<", RPrimLt)
  , ("<=", RPrimLe)
  , (">=", RPrimGe)
  , (">", RPrimGt)

  , ("all", RPrimAll)
  , ("any", RPrimAny)
  , ("sum", RPrimSum)
  , ("prod", RPrimProd)
  , ("max", RPrimMax)
  , ("min", RPrimMin)
  , ("range", RPrimRange)

  , ("Arg", RPrimArg)
  , ("Conj", RPrimConj)
  , ("Im", RPrimIm)
  , ("Mod", RPrimModulus)
  , ("Re", RPrimReal)

  -- Things that were not explicitly included but nice for smooth translation.
  , ("!", RPrimNot)
  , (":", RPrimColon)
  , ("<-", RPrimAssign)
  , ("<<-", RPrimSuperAssign)
  , ("~", RPrimForm)
  , ("?", RPrimHelp)
  , ("[[", RPrimVecProj)
  , ("[", RPrimVecSub)
  ]

rPrimToString :: RPrim -> String
rPrimToString prim =
  case map fst $ filter ((== prim) . snd) primNames of
    [] -> error $ "rPrimToString: failed to account for " ++ show prim
    (str : []) -> str
    (_ : _) -> error $ "rPrimToString: too many matches for " ++ show prim

idFromRPrim :: RPrim -> Ident
idFromRPrim prim =
  let name = rPrimToString prim in
    Ident { idName = name, idPkg = Just "prim", idAnnot = Nothing }

idStupidFromRPrim :: RPrim -> Ident
idStupidFromRPrim prim =
  let name = rPrimToString prim in
    Ident { idName = name, idPkg = Nothing, idAnnot = Nothing }


