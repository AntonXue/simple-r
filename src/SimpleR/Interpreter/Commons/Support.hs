module SimpleR.Interpreter.Commons.Support where

import qualified Data.Map as M
import qualified Data.Set as S

import SimpleR.Language
import SimpleR.Smt

data Type = IntTy | DoubleTy | ComplexTy | BoolTy | StringTy
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

data Vector =
    IntVec [Int]
  | DoubleVec [Double]
  | ComplexVec [Complex]
  | BoolVec [Bool]
  | StringVec [String]
  | NilVec
  | SymVec SmtIdent Type
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

data LoopState =
    LoopStateCond
  | LoopStateBody
  deriving (Eq, Show, Read)

data Cont =
    EvalCont Expr
  | ReturnCont MemRef
  | SeqCont [Expr]
  | BranchCont Expr Expr
  | LoopCont Expr Expr LoopState

  | AssignCont Ident
  | SupAssignCont Ident
  | LamACont (Maybe MemRef) [(Arg, MemRef)] (Maybe Arg) [Arg]
  | LamBCont MemRef

  | UpdateCont MemRef
  | ArgCont [Arg]
  | AttrCont (Maybe MemRef) (Maybe Expr)
  deriving (Eq, Show, Read)

data SymMems = SymMems
  { symMemsList :: [MemRef]
  } deriving (Eq, Show, Read)

data Pures = Pures
  { puresSet :: S.Set Ident
  } deriving (Eq, Show, Read)

data State = State
  { stStack :: Stack
  , stHeap :: Heap
  , stBaseEnvMem :: MemRef
  , stGlobalEnvMem :: MemRef
  , stSymMems :: SymMems
  , stPures :: Pures
  , stConstr :: Constraint
  , stFreshCount :: Int
  , stPredUnique :: Int
  , stUnique :: Int
  } deriving (Eq, Show, Read)

