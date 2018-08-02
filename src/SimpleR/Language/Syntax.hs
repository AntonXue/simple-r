module SimpleR.Language.Syntax where

import qualified Data.Complex as C

type Complex = C.Complex Double

data IdentAnnot = IdentAnnot deriving (Ord, Eq, Show, Read)

data Ident = Ident
  { id_pkg :: Maybe String
  , id_name :: String
  , id_annot :: IdentAnnot
  } deriving (Ord, Eq, Show, Read)

data TickAnnot = TickAnnot deriving (Eq, Show, Read)

data Tick = Tick
  { tick_annot :: TickAnnot
  } deriving (Eq, Show, Read)

data MemRef = MemRef
  { mem_addr :: Int
  } deriving (Ord, Eq, Show, Read)

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
  | Mem MemRef
  | Const Const
  | Seq [Expr]
  | LambdaAbs [Param] Expr
  | LambdaApp Expr [Arg]
  | NativeLambdaApp Ident [Arg]
  | Return Expr
  | Assign Expr Expr
  | SuperAssign Expr Expr
  | If Expr Expr Expr
  | While Expr Expr
  | Break
  | Next
  | Error
  | Tix Tick Expr
  deriving (Eq, Show, Read)


