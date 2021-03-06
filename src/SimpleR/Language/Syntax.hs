module SimpleR.Language.Syntax where

import qualified Data.Complex as C

type Complex = C.Complex Double

data IdentAnnot = IdentAnnot deriving (Ord, Eq, Show, Read)

data Ident = Ident
  { idName :: String
  , idPkg :: Maybe String
  , idAnnot :: Maybe IdentAnnot
  } deriving (Ord, Eq, Show, Read)

data TickAnnot = TickAnnot  deriving (Eq, Show, Read)

data Tick = Tick
  { tickAnnot :: TickAnnot
  }  deriving (Eq, Show, Read)

data Const =
    IntConst Int
  | DoubleConst Double
  | ComplexConst Complex
  | BoolConst Bool
  | StringConst String
  | NilConst
  | NaConst
   deriving (Eq, Show, Read)

data Param =
    Param Ident
  | Default Ident Expr
  | VarParam
   deriving (Eq, Show, Read)

data Arg =
    Arg Expr
  | Named Ident Expr
  | VarArg
   deriving (Eq, Show, Read)

data Expr =
    Var Ident
  | Const Const
  | Seq [Expr]
  | LamAbs [Param] Expr
  | LamApp Expr [Arg]
  | NativeLamApp Ident [Ident]
  | Return Expr
  | Assign Expr Expr

  | VecInd Expr [Arg]
  | VecIndAssign Expr [Arg] Expr
  | VecSub Expr [Arg]
  | VecSubAssign Expr [Arg] Expr
  | ListName Expr Expr
  | ListNameAssign Expr Expr Expr
  | ObjAttr Expr Expr
  | ObjAttrAssign Expr Expr Expr

  | If Expr Expr Expr
  | While Expr Expr
  | Break
  | Next

  | Error
  | Tix Tick Expr
   deriving (Eq, Show, Read)

data Program = Program [Expr]  deriving (Eq, Show, Read)

