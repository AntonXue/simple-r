module SimpleR.Language.Syntax where

import qualified Data.Complex as C

data RInt =
    RInt Int
  | NAInt
  deriving (Eq, Show, Read)

data RDouble =
    RDouble Double
  | NADouble
  deriving (Eq, Show, Read)

data RComplex =
    RComplex (C.Complex Double)
  | NAComplex
  deriving (Eq, Show, Read)

data RBool =
    RBool Int
  | NABool
  deriving (Eq, Show, Read)

data RString =
    RString String
  | NAString
  deriving (Eq, Show, Read)

data IdentAnnot = IdentAnnot deriving (Eq, Show, Read)

data Ident = Ident
  { id_pkg :: Maybe RString
  , id_name :: RString
  , id_annot :: IdentAnnot
  } deriving (Eq, Show, Read)

data TickAnnot = TickAnnot deriving (Eq, Show, Read)

data Tick = Tick
  { tick_annot :: TickAnnot
  } deriving (Eq, Show, Read)

data MemRef = MemRef
  { mem_addr :: Int
  } deriving (Eq, Show, Read)

data Const =
    IntConst RInt
  | DoubleCont RDouble
  | ComplexConst RComplex
  | BoolConst RBool
  | StringConst RString
  | NilConst
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


-- Some type classes

-- Numeric

class Numeric a
instance Numeric RInt
instance Numeric RDouble
instance Numeric RComplex
instance Numeric RBool


