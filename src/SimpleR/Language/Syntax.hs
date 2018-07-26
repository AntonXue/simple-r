module SimpleR.Language.Syntax where

import qualified Data.Complex as C

data SInt =
    SInt Int
  | SNaInt
  deriving (Ord, Eq, Show, Read)

data SDouble =
    SDouble Double
  | SNaDouble
  deriving (Ord, Eq, Show, Read)

data SComplex =
    SComplex (C.Complex Double)
  | SNaComplex
  deriving (Eq, Show, Read)

data SBool =
    SBool Int
  | SNaBool
  deriving (Ord, Eq, Show, Read)

data SString =
    SString String
  | SNaString
  deriving (Ord, Eq, Show, Read)

data SIdentAnnot = SIdentAnnot deriving (Ord, Eq, Show, Read)

data SIdent = SIdent
  { sid_pkg :: Maybe SString
  , sid_name :: SString
  , sid_annot :: SIdentAnnot
  } deriving (Ord, Eq, Show, Read)

data STickAnnot = STickAnnot deriving (Eq, Show, Read)

data STick = STick
  { stick_annot :: STickAnnot
  } deriving (Eq, Show, Read)

data SMemRef = SMemRef
  { smem_addr :: Int
  } deriving (Ord, Eq, Show, Read)

data SConst =
    SIntConst SInt
  | SDoubleCont SDouble
  | SComplexConst SComplex
  | SBoolConst SBool
  | SStringConst SString
  | SNilConst
  deriving (Eq, Show, Read)

data SParam =
    SParam SIdent
  | SDefault SIdent SExpr
  | SVarParam
  deriving (Eq, Show, Read)

data SArg =
    SArg SExpr
  | SNamed SIdent SExpr
  | SVarArg
  deriving (Eq, Show, Read)

data SExpr =
    SVar SIdent
  | SMem SMemRef
  | SConst SConst
  | SSeq [SExpr]
  | SLambdaAbs [SParam] SExpr
  | SLambdaApp SExpr [SArg]
  | SNativeLambdaApp SIdent [SArg]
  | SReturn SExpr
  | SAssign SExpr SExpr
  | SSuperAssign SExpr SExpr
  | SIf SExpr SExpr SExpr
  | SWhile SExpr SExpr
  | SBreak
  | SNext
  | SError
  | STix STick SExpr
  deriving (Eq, Show, Read)


-- Some type classes

-- Numeric

class SNumeric a
instance SNumeric SInt
instance SNumeric SDouble
instance SNumeric SComplex
instance SNumeric SBool


