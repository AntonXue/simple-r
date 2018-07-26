module SimpleR.Interpreter.Commons.Support where

import qualified Data.Map as M

import SimpleR.Language
import SimpleR.Smt

data RType = RInt | RDouble | RComplex | RBool | RString
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
  | PureObj MemRef Expr
  | DataObj Value Attributes
  deriving (Eq, Show, Read)

data Value =
    VecVal RVector
  | RefsVal [MemRef]
  | FuncVal MemRef [Param] Expr
  | EnvVal Env
  deriving (Eq, Show, Read)

data RVector =
    IntVec [RInt]
  | DoubleVec [RDouble]
  | ComplexVec [RComplex]
  | BoolVec [RBool]
  | StringVec [RString]
  | SymVec SymVector
  deriving (Eq, Show, Read)

data SymVector =
    SymVector SmtIdent RType PathCons [SymVector]
  deriving (Eq, Show, Read)

data Attributes = Attributes
  { attrs_map :: M.Map RString MemRef
  } deriving (Eq, Show, Read)

data PathCons = PathCons
  { path_list :: [SmtExpr]
  } deriving (Eq, Show, Read)

data Stack = Stack
  { stack_list :: [Frame]
  } deriving (Eq, Show, Read)

data Frame = Frame
  { frame_env_mem :: MemRef
  , frame_slot :: Slot
  } deriving (Eq, Show, Read)

data Slot =
    EvalSlot Expr
  | ReturnSlot MemRef
  | SeqSlot [Expr]
  | BranchSlot Expr Expr
  | LoopSlot Expr Expr (Maybe MemRef)

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

