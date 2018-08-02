module SimpleR.Interpreter.Commons.Support where

import qualified Data.Map as M

import SimpleR.Language
import SimpleR.Smt

data Type = IntTy | DoubleTy | ComplexTy | BoolTy | StringTy
  deriving (Eq, Show, Read)

data Env = Env
  { env_map :: M.Map Ident MemRef
  , env_pred_mem :: MemRef
  } deriving (Eq, Show, Read)

data Heap = Heap
  { heap_map :: M.Map MemRef HeapObj
  , heap_next_mem :: MemRef
  } deriving (Eq, Show, Read)

data HeapObj =
    PromiseObj MemRef Expr
  | DataObj Value Attributes
  deriving (Eq, Show, Read)

data Value =
    VecVal Vector
  | RefsVal [MemRef]
  | FuncVal MemRef [Param] Expr
  | EnvVal Env
  deriving (Eq, Show, Read)

data Vector =
    IntVec [Int]
  | DoubleVec [Double]
  | ComplexVec [Complex]
  | BoolVec [Bool]
  | StringVec [String]
  | NilVec
  | SymVec SmtIdent Type Constraint
  deriving (Eq, Show, Read)

data Attributes = Attributes
  { attrs_map :: M.Map String MemRef
  } deriving (Eq, Show, Read)

data Constraint = Constraint
  { const_list :: [SmtExpr]
  } deriving (Eq, Show, Read)

data Stack = Stack
  { stack_list :: [Frame]
  } deriving (Eq, Show, Read)

data Frame = Frame
  { frame_env_mem :: MemRef
  , frame_slot :: Slot
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
  { smems_list :: [MemRef]
  } deriving (Eq, Show, Read)

data State = State
  { st_stack :: Stack
  , st_heap :: Heap
  , st_base_env_mem :: MemRef
  , st_glbl_env_mem :: MemRef
  , st_sym_mems :: SymMems
  , st_fresh_count :: Int
  , st_pred_unique :: Int
  , st_unique :: Int
  } deriving (Eq, Show, Read)

