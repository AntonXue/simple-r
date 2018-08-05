module SimpleR.Interpreter.Natives.RPrimitives
  (
  ) where

import SimpleR.Language

mkPrimId :: String -> Ident
mkPrimId str =
  Ident { idName = str, idPkg = Just "prim", idAnnot = IdentAnnot }

data RPrim =
    PrimAnyNa
  | PrimAsCharacter
  | PrimAsComplex
  | PrimAsDouble
  | PrimAsEnvironment
  | PrimAsInteger
  | PrimAsLogical
  | PrimAsCall
  | PrimAsNumeric
  | PrimAsRaw
  | PrimC
  | PrimDim
  | PrimDimArrow
  | PrimDimNames
  | PrimDimNamesAssign
  | PrimIsArray
  | PrimIsFinite
  | PrimIsInfinite
  | PrimIsMatrix
  | PrimIsNa
  | PrimIsNan
  | PrimIsNumeric
  | PrimLength
  | PrimLengthAssign
  | PrimLevelsAssign
  | PrimNames
  | PrimNamesAssign
  | PrimRep
  | PrimSeqInt
  | PrimXtfrm

  | PrimAbs
  | PrimSign
  | PrimSqrt
  | PrimFloor
  | PrimCeiling
  | PrimExp
  | PrimExpm1
  | PrimLog1p
  | PrimLog10
  | PrimLog2
  | PrimCos
  | PrimSin
  | PrimTan
  | PrimAcos
  | PrimAsin
  | PrimAtan
  | PrimCosh
  | PrimSinh
  | PrimTanh
  | PrimAcosh
  | PrimAsinh
  | PrimAtanh
  | PrimCosPi
  | PrimSinPi
  | PrimTanPi
  | PrimGamma
  | PrimLGamma
  | PrimDiGamma
  | PrimTriGamma
  | PrimCumSum
  | PrimCumProd
  | PrimCumMax
  | PrimCumMin

  | PrimPlus
  | PrimMinus
  | PrimMult
  | PrimDiv
  | PrimPow
  | PrimMod
  | PrimIntDiv
  | PrimAnd
  | PrimOr
  | PrimEq
  | PrimNeq
  | PrimLt
  | PrimLe
  | PrimGe
  | PrimGt

  | PrimAll
  | PrimAny
  | PrimSum
  | PrimProd
  | PrimMax
  | PrimMin
  | PrimRange

  | PrimArg
  | PrimConj
  | PrimIm
  | PrimModulus
  | PrimReal
  deriving (Ord, Eq, Show, Read)


-- https://github.com/wch/r-source/blob/a6d3738aeb73c748f27a94a2c97bb5750c6e01bf/src/library/base/R/zzz.R#L150-#L210
primNames :: [(String, RPrim)]
primNames =
  [ ("anyNA", PrimAnyNa)
  , ("as.character", PrimAsCharacter)
  , ("as.complex", PrimAsComplex)
  , ("as.double", PrimAsDouble)
  , ("as.environment", PrimAsEnvironment)
  , ("as.integer", PrimAsInteger)
  , ("as.logical", PrimAsLogical)
  , ("as.call", PrimAsCall)
  , ("as.numeric", PrimAsNumeric)
  , ("as.raw", PrimAsRaw)
  , ("c", PrimC)
  , ("dim", PrimDim)
  , ("dim<-", PrimDimArrow)
  , ("dimnames", PrimDimNames)
  , ("dimnames<-", PrimDimNamesAssign)
  , ("is.array", PrimIsArray)
  , ("is.finite", PrimIsFinite)
  , ("is.infinite", PrimIsInfinite)
  , ("is.matrix", PrimIsMatrix)
  , ("is.na", PrimIsNa)
  , ("is.nan", PrimIsNan)
  , ("is.numeric", PrimIsNumeric)
  , ("length", PrimLength)
  , ("length<-", PrimLengthAssign)
  , ("levels<-", PrimLevelsAssign)
  , ("names", PrimNames)
  , ("names<-", PrimNamesAssign)
  , ("rep", PrimRep)
  , ("seq.int", PrimSeqInt)
  , ("xtfrm", PrimXtfrm)

  , ("abs", PrimAbs)
  , ("sign", PrimSign)
  , ("sqrt", PrimSqrt)
  , ("floor", PrimFloor)
  , ("ceiling", PrimCeiling)
  , ("exp", PrimExp)
  , ("expm1", PrimExpm1)
  , ("log1p", PrimLog1p)
  , ("log10", PrimLog10)
  , ("log2", PrimLog2)
  , ("cos", PrimCos)
  , ("sin", PrimSin)
  , ("tan", PrimTan)
  , ("acos", PrimAcos)
  , ("asin", PrimAsin)
  , ("atan", PrimAtan)
  , ("cosh", PrimCosh)
  , ("sinh", PrimSinh)
  , ("tanh", PrimTanh)
  , ("acosh", PrimAcosh)
  , ("asinh", PrimAsinh)
  , ("atanh", PrimAtanh)
  , ("cospi", PrimCosPi)
  , ("sinpi", PrimSinPi)
  , ("tanpi", PrimTanPi)
  , ("gamma", PrimGamma)
  , ("lgamma", PrimLGamma)
  , ("digamma", PrimDiGamma)
  , ("trigamma", PrimTriGamma)
  , ("cumsum", PrimCumSum)
  , ("cumprod", PrimCumProd)
  , ("cummax", PrimCumMax)
  , ("cummin", PrimCumMin)

  , ("+", PrimPlus)
  , ("-", PrimMinus)
  , ("*", PrimMult)
  , ("/", PrimDiv)
  , ("^", PrimPow)
  , ("%%", PrimMod)
  , ("%/%", PrimIntDiv)
  , ("&", PrimAnd)
  , ("|", PrimOr)
  , ("==", PrimEq)
  , ("!=", PrimNeq)
  , ("<", PrimLt)
  , ("<=", PrimLe)
  , (">=", PrimGe)
  , (">", PrimGt)

  , ("all", PrimAll)
  , ("any", PrimAny)
  , ("sum", PrimSum)
  , ("prod", PrimProd)
  , ("max", PrimMax)
  , ("min", PrimMin)
  , ("range", PrimRange)

  , ("Arg", PrimArg)
  , ("Conj", PrimConj)
  , ("Im", PrimIm)
  , ("Mod", PrimModulus)
  , ("Re", PrimReal)
  ]

