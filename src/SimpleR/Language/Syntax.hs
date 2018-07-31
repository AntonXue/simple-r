module SimpleR.Language.Syntax where

import qualified Data.Complex as C

type Complex = C.Complex Double

data SIdentAnnot = SIdentAnnot deriving (Ord, Eq, Show, Read)

data SIdent = SIdent
  { sid_pkg :: Maybe String
  , sid_name :: String
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
    SIntConst Int
  | SDoubleConst Double
  | SComplexConst (Complex)
  | SBoolConst Bool
  | SStringConst String
  | SNilConst
  | SNaConst
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


