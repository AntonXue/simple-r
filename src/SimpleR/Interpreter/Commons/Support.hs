module SimpleR.Interpreter.Commons.Support where

import qualified Data.Map as M
import qualified Data.Set as S

import SimpleR.Language
import SimpleR.Smt
import SimpleR.Interpreter.Commons.Vector

------
-- This files describes the internal data structures used during
-- symbolic execution.
-- A separate src/SimpleR/Interpreter/Commons/SupportUtils.hs contains
-- the utility functions that correspond to this file.


-- A memory address is just an integer
data MemAddr = MemAddr
  { memAddr :: Int
  } deriving (Ord, Eq, Show, Read)

-- This is the evaluation context / redex, though that's terminology abuse.
data Redex =
    -- Environemnt memory and expression
    EvalRed MemAddr Expr
    -- Address points to value on the heap
  | ResultRed MemAddr
    -- Used when we need to 'juggle' things on the continuation stack
  | BlankRed
  deriving (Eq, Show, Read)

-- Environments map identifiers to memory, and also contain a parent pointer.
data Env = Env
  { envMap :: M.Map Ident MemAddr
  , envPredMem :: MemAddr
  } deriving (Eq, Show, Read)

-- Heaps map addresses to heap objects (values)
data Heap = Heap
  { heapMap :: M.Map MemAddr HeapObj
    -- The next 'free' memory to alloc to
  , heapNextMem :: MemAddr
  } deriving (Eq, Show, Read)

-- The things that can be stored on the heap.
data HeapObj =
    -- Thunks, and also includes environment address
    PromiseObj MemAddr Expr
    -- Holds the values and attribute pairing
  | DataObj Value Attributes
  deriving (Eq, Show, Read)

-- The values in Simple-R
data Value =
    -- We may have vectors
    VecVal Vector
    -- This is to really emulate lists, and also used for variadics
  | RefsVal [MemAddr]
    -- We may have functions:
      -- memory of environment in which it was lexically defined
      -- formal parameters
      -- expression body
  | FunVal MemAddr [Param] Expr
    -- Environments are stored on the heap
  | EnvVal Env
  deriving (Eq, Show, Read)

-- Attributes may be attached to values, but they only affect vectors
data Attributes = Attributes
  { attrsMap :: M.Map Ident MemAddr
  } deriving (Eq, Show, Read)

-- Constraints and stuff -- subject to change, a lot!!!
data Constraint = Constraint
  { constrAssertList :: [SmtExpr]
  , constrPreList :: [SmtCmd]
  , constrPostList :: [SmtCmd]
  } deriving (Eq, Show, Read)

-- The stack of continuation frames
data Stack = Stack
  { stackList :: [Frame]
  } deriving (Eq, Show, Read)

-- Each frame contains the memory address relevant to it and the continuation
data Frame = Frame
  { frameEnvMem :: MemAddr
  , frameCont :: Cont
  } deriving (Eq, Show, Read)

-- Are we currently doing the loop's condition or the loop's body?
data LoopConfig =
    LoopConfigCond
  | LoopConfigBody
  deriving (Eq, Show, Read)

-- Continuations
data Cont =
    -- This holds an expression, but not to be confused with EvalRed!
    -- Because R has sequenced expressions, this was the engineering decision.
    ExprCont Expr
    -- Holds the true and false branches of an if statement while the
    -- condition is under evaluation
  | BranchCont Expr Expr
  -- We overload this to hold the condition and the expression.
  -- LoopConfig is used to tell which one is under evaluation in the EvalRed.
  | LoopCont Expr Expr LoopConfig
  -- Holds the identifier while we are performing the evaluation of the RHS.
  | AssignCont Ident

  -- Eager function evaluation
    -- The first (Maybe MemAddr) holds the address of the function.
    -- The [(Arg, MemAddr)] are the eval'd arguments paired with the originals
    -- The (Maybe Arg) is the form of the current argument under evaluation
    -- [Arg] are the unprocessed arguments
  | LamACont (Maybe MemAddr) [(Arg, MemAddr)] (Maybe Arg) [Arg]

  -- A marker for a function call and returns.
  -- Holds the address of the function.
  | LamBCont MemAddr

  -- Everything below this is currently not really used
  -- Unused, but existing
  | SupAssignCont Ident

  -- Really lazy evaluation.
  | UpdateCont MemAddr
  | ArgCont [Arg]

  -- Arguments
  | AttrCont (Maybe MemAddr) (Maybe Expr)
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
  , stBaseEnvMem :: MemAddr
  , stGlobalEnvMem :: MemAddr
  , stPures :: Pures
  , stFreshCount :: Int
  , stPredUnique :: Int
  , stUnique :: Int
  } deriving (Eq, Show, Read)

