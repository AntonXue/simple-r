module SimpleR.Interpreter.Commons.Support where

import qualified Data.Map as M

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
  , frameSlot :: Slot
  } deriving (Eq, Show, Read)

data LoopState =
    LoopCond
  | LoopBody
  deriving (Eq, Show, Read)

data Slot =
    EvalSlot Expr
  | ReturnSlot MemRef
  | SeqSlot [Expr]
  | BranchSlot Expr Expr
  | LoopSlot Expr Expr LoopState

  | AssignSlot Ident
  | SupAssignSlot Ident
  | LambdaASlot (Maybe MemRef) [(Arg, MemRef)] (Maybe Arg) [Arg]
  | LambdaBSlot MemRef

  | UpdateSlot MemRef
  | ArgSlot [Arg]
  | AttrSlot (Maybe MemRef) (Maybe Expr)
  deriving (Eq, Show, Read)

data SymMems = SymMems
  { symMemsList :: [MemRef]
  } deriving (Eq, Show, Read)

data State = State
  { stStack :: Stack
  , stHeap :: Heap
  , stBaseEnvMem :: MemRef
  , stGlobalEnvMem :: MemRef
  , stSymMems :: SymMems
  , stConstr :: Constraint
  , stFreshCount :: Int
  , stPredUnique :: Int
  , stUnique :: Int
  } deriving (Eq, Show, Read)

