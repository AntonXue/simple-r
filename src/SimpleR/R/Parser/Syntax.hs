module SimpleR.R.Parser.Syntax where

import qualified Data.Complex as C


data RSource = Source
  { srcFile :: String
  , srcLine :: Int
  , srcCol :: Int
  } deriving (Eq, Show, Read)

data RIdentAnnot = RIdentAnnot deriving (Eq, Show, Read)

data RIdent = RIdent
  { ridPkg :: Maybe String
  , ridName :: String
  , ridSrc :: Maybe RSource
  , ridAnnot :: Maybe RIdentAnnot
  } deriving (Eq, Show, Read)

data RNumeric =
    RNumInt Int
  | RNumNaInt
  | RNumFloat Double
  | RNumNaFloat
  | RNumComplex (C.Complex Double)
  | RNumNaComplex
  deriving (Eq, Show, Read)

data RBool =
    RBool Bool
  | RNaBool
  deriving (Eq, Show, Read)

data RString =
    RString String
  | RNaString
  deriving (Eq, Show, Read)

data RUnOp =
    RUMinus
  | RUPlus
  | RUNot
  | RUForm
  | RUHelp
  deriving (Eq, Show, Read)

data RBinOp =
    RPlus | RMinus | RMult | RDiv | RPow | RMod | RIntDiv
  | RGt | RGe | RLt | RLe | REq | RNeq
  | RAnd | RAndVec | ROr | ROrVec
  | RMatrixMult | ROuterProd | RKronProd
  | RAssign | RSuperAssign
  | RObjAttr
  | RRange
  | RForm
  | RGetPackage
  | RGetPackageInt
  | RMatch
  | RHelp
  deriving (Eq, Show, Read)

data RParam =
    RParam RIdent
  | RDefault RIdent RExpr
  | RVarParam
  deriving (Eq, Show, Read)

data RArg =
    RExprArg RExpr
  | RIdentAssign RIdent RExpr
  | RIdentAssignEmpty RIdent
  | RStringAssign RString RExpr
  | RStringAssignEmpty RString
  | RNullAssign RExpr
  | RNullAssignEmpty
  | RVarArg
  deriving (Eq, Show, Read)

data RConst =
    RNumConst RNumeric
  | RStrConst RString
  | RBoolConst RBool
  | RNaConst
  deriving (Eq, Show, Read)

data RExpr =
    RConst RConst
  | RVar RIdent
  | RNull
  | RUnOp RUnOp RExpr
  | RBinOp RBinOp RExpr RExpr
  | RFunCall RExpr [RArg]
  | RFunDef [RParam] RExpr
  | RSeq [RExpr]
  | RVecProj RExpr [RArg]
  | RVecSub RExpr [RArg]
  | RIf RExpr RExpr
  | RIfElse RExpr RExpr RExpr
  | RFor RIdent RExpr RExpr
  | RWhile RExpr RExpr
  | RRepeat RExpr
  | RNext
  | RBreak
  deriving (Eq, Show, Read)

data RProgram = RProgram [RExpr] deriving (Eq, Show, Read)

