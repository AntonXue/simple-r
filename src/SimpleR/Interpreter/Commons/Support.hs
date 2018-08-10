module SimpleR.Interpreter.Commons.Support where

import qualified Data.Map as M
import qualified Data.Set as S

import SimpleR.Language
import SimpleR.Smt
import SimpleR.Interpreter.Commons.Vector

data MemRef = MemRef
  { memAddr :: Int
  } deriving (Ord, Eq, Show, Read)

data Redex =
    EvalRed MemRef Expr
  | ResultRed MemRef
  | BlankRed
  deriving (Eq, Show, Read)

data Env = Env
  { envMap :: M.Map Ident MemRef
  , envPredMem :: MemRef
  } deriving (Eq, Show, Read)

data Heap = Heap
  { heapMap :: M.Map MemRef HeapObj
  , heapNextMem :: MemRef
  } deriving (Eq, Show, Read)

data HeapObj =
    PromiseObj MemRef Expr
  | DataObj Value Attributes
  deriving (Eq, Show, Read)

data Value =
    VecVal Vector
  | RefsVal [MemRef]
  | FunVal MemRef [Param] Expr
  | EnvVal Env
  deriving (Eq, Show, Read)

data Attributes = Attributes
  { attrsMap :: M.Map String MemRef
  } deriving (Eq, Show, Read)

data Constraint = Constraint
  { constrList :: [SmtExpr]
  } deriving (Eq, Show, Read)

data Stack = Stack
  { stackList :: [Frame]
  } deriving (Eq, Show, Read)

data Frame = Frame
  { frameEnvMem :: MemRef
  , frameCont :: Cont
  } deriving (Eq, Show, Read)

data LoopConfig =
    LoopConfigCond
  | LoopConfigBody
  deriving (Eq, Show, Read)

data Cont =
    ExprCont Expr
  | BranchCont Expr Expr
  | LoopCont Expr Expr LoopConfig
  | AssignCont Ident

  -- Eager function evaluation
  | LamACont (Maybe MemRef) [(Arg, MemRef)] (Maybe Arg) [Arg]
  | LamBCont MemRef

  -- Unused, but existing
  | SupAssignCont Ident

  -- Really lazy evaluation.
  | UpdateCont MemRef
  | ArgCont [Arg]

  -- Arguments
  | AttrCont (Maybe MemRef) (Maybe Expr)
  deriving (Eq, Show, Read)

data Pures = Pures
  { puresSet :: S.Set Ident
  } deriving (Eq, Show, Read)

data State = State
  { stRedex :: Redex
  , stStack :: Stack
  , stHeap :: Heap
  , stConstr :: Constraint
  --
  , stBaseEnvMem :: MemRef
  , stGlobalEnvMem :: MemRef
  , stPures :: Pures
  , stFreshCount :: Int
  , stPredUnique :: Int
  , stUnique :: Int
  } deriving (Eq, Show, Read)

