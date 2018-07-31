module SimpleR.Interpreter.Commons.Support where

import qualified Data.Map as M

import SimpleR.Language
import SimpleR.Smt

data Type = IntTy | DoubleTy | ComplexTy | BoolTy | StringTy
  deriving (Eq, Show, Read)

data Env = Env
  { env_map :: M.Map SIdent SMemRef
  , env_pred_mem :: SMemRef
  } deriving (Eq, Show, Read)

data Heap = Heap
  { heap_map :: M.Map SMemRef HeapObj
  , heap_next_mem :: SMemRef
  } deriving (Eq, Show, Read)

data HeapObj =
    PromiseObj SMemRef SExpr
  | DataObj Value Attributes
  deriving (Eq, Show, Read)

data Value =
    VecVal Vector
  | RefsVal [SMemRef]
  | FuncVal SMemRef [SParam] SExpr
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
  { attrs_map :: M.Map String SMemRef
  } deriving (Eq, Show, Read)

data Constraint = Constraint
  { const_list :: [SmtExpr]
  } deriving (Eq, Show, Read)

data Stack = Stack
  { stack_list :: [Frame]
  } deriving (Eq, Show, Read)

data Frame = Frame
  { frame_env_mem :: SMemRef
  , frame_slot :: Slot
  } deriving (Eq, Show, Read)

data Slot =
    EvalSlot SExpr
  | ReturnSlot SMemRef
  | SeqSlot [SExpr]
  | BranchSlot SExpr SExpr
  | LoopSlot SExpr SExpr (Maybe SMemRef)

  | AssignSlot SIdent
  | SupAssignSlot SIdent
  | LambdaASlot (Maybe SMemRef) [(SArg, SMemRef)] (Maybe SArg) [SArg]
  | LambdaBSlot SMemRef

  | UpdateSlot SMemRef
  | ArgSlot [SArg]
  | AttrSlot (Maybe SMemRef) (Maybe SExpr)
  deriving (Eq, Show, Read)

data SymMems = SymMems
  { smems_list :: [SMemRef]
  } deriving (Eq, Show, Read)

data State = State
  { st_stack :: Stack
  , st_heap :: Heap
  , st_base_env_mem :: SMemRef
  , st_glbl_env_mem :: SMemRef
  , st_sym_mems :: SymMems
  , st_fresh_count :: Int
  , st_pred_unique :: Int
  , st_unique :: Int
  } deriving (Eq, Show, Read)

