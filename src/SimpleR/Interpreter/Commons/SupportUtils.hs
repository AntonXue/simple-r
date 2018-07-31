module SimpleR.Interpreter.Commons.SupportUtils where

import qualified Data.Map as M

import SimpleR.Language
import SimpleR.Interpreter.Commons.Support

mapInsertList :: (Ord k) => [(k, v)] -> M.Map k v -> M.Map k v
mapInsertList kvs map = foldl (\m (k, v) -> M.insert k v m) map kvs

mapDeleteList :: (Ord k) => [k] -> M.Map k v -> M.Map k v
mapDeleteList ks map = foldl (\m k -> M.delete k m) map ks

-- Memories
baseMem :: SMemRef
baseMem = memNull

globalMem :: SMemRef
globalMem = memNull

-- Identifier
idVariadic :: SIdent
idVariadic = idFromString "..."

freshId :: State -> (SIdent, State)
freshId = undefined

freshIdSeeded :: String -> State -> (SIdent, State)
freshIdSeeded = undefined

-- Vector conversion
vecFromSConst :: SConst -> Vector
vecFromSConst (SIntConst int) = IntVec [int]
vecFromSConst (SDoubleConst double) = DoubleVec [double]
vecFromSConst (SComplexConst complex) = ComplexVec [complex]
vecFromSConst (SBoolConst bool) = BoolVec [bool]
vecFromSConst (SStringConst str) = StringVec [str]
vecFromSConst (SNaConst) = NilVec

-- Environment
envEmpty :: Env
envEmpty = Env {env_map = M.empty, env_pred_mem = globalMem}

envInsert :: SIdent -> SMemRef -> Env -> Env
envInsert id mem env = envInsertList [(id, mem)] env

envInsertList :: [(SIdent, SMemRef)] -> Env -> Env
envInsertList kvs env = env {env_map = mapInsertList kvs $ env_map env}

envDelete :: SIdent -> Env -> Env
envDelete id env = envDeleteList [id] env

envDeleteList :: [SIdent] -> Env -> Env
envDeleteList ids env = env {env_map = mapDeleteList ids $ env_map env}

envAssignPred :: SMemRef -> Env -> Env
envAssignPred mem env = env {env_pred_mem = mem}

envBinds :: Env -> [(SIdent, SMemRef)]
envBinds env = M.toList $ env_map env

-- Heap
heapEmpty :: Heap
heapEmpty = Heap {heap_map = M.empty, heap_next_mem = memNext memNull}

heapInsert :: SMemRef -> HeapObj -> Heap -> Heap
heapInsert mem hobj heap = heapInsertList [(mem, hobj)] heap

heapInsertList :: [(SMemRef, HeapObj)] -> Heap -> Heap
heapInsertList kvs heap = heap {heap_map = mapInsertList kvs $ heap_map heap}

heapDelete :: SMemRef -> Heap -> Heap
heapDelete mem heap = heapDeleteList [mem] heap

heapDeleteList :: [SMemRef] -> Heap -> Heap
heapDeleteList mems heap =
  heap {heap_map = mapDeleteList mems $ heap_map heap}

heapAlloc :: HeapObj -> Heap -> (SMemRef, Heap)
heapAlloc hobj heap =
  let used_mem = heap_next_mem heap in
  let heap2 = heapInsert used_mem hobj heap in
    (used_mem, heap2 {heap_next_mem = memNext used_mem})

heapAllocList :: [HeapObj] -> Heap -> ([SMemRef], Heap)
heapAllocList [] heap = ([], heap)
heapAllocList (hobj : hobjs) heap =
  let (used_mem, heap2) = heapAlloc hobj heap in
  let (used_mems, heap3) = heapAllocList hobjs heap2 in
    (used_mem : used_mems, heap3)

heapAllocSConst :: SConst -> Heap -> (SMemRef, Heap)
heapAllocSConst const heap =
  heapAlloc (DataObj (VecVal (vecFromSConst const)) attrsEmpty) heap

heapBinds :: Heap -> [(SMemRef, HeapObj)]
heapBinds heap = M.toList $ heap_map heap

-- Attributes
attrsEmpty :: Attributes
attrsEmpty = Attributes {attrs_map = M.empty}

attrsInsert :: String -> SMemRef -> Attributes -> Attributes
attrsInsert str mem attrs = attrsInsertList [(str, mem)] attrs

attrsInsertList :: [(String, SMemRef)] -> Attributes -> Attributes
attrsInsertList kvs attrs =
  attrs {attrs_map = mapInsertList kvs $ attrs_map attrs}

attrsDelete :: String -> Attributes -> Attributes
attrsDelete str attrs = attrsDeleteList [str] attrs

attrsDeleteList :: [String] -> Attributes -> Attributes
attrsDeleteList strs attrs =
  attrs {attrs_map = mapDeleteList strs $ attrs_map attrs}

attrsBinds :: Attributes -> [(String, SMemRef)]
attrsBinds attrs = M.toList $ attrs_map attrs

-- Stack
stackEmpty :: Stack
stackEmpty = Stack {stack_list = []}

stackPush :: Frame -> Stack -> Stack
stackPush frame stack = stackPushList [frame] stack

stackPushList :: [Frame] -> Stack -> Stack
stackPushList frames stack = stack {stack_list = frames ++ stack_list stack }

stackPop :: Stack -> Maybe (Frame, Stack)
stackPop stack = case stack_list stack of
  [] -> Nothing
  (frame : frames) -> Just (frame, stack {stack_list = frames})

stackPopV :: Stack -> Maybe (Slot, SMemRef, Stack)
stackPopV stack = case stackPop stack of
  Just (frame, stack2) -> Just (frame_slot frame, frame_env_mem frame, stack2)
  Nothing -> Nothing

stackPopV2 :: Stack -> Maybe (Slot, SMemRef, Slot, SMemRef, Stack)
stackPopV2 stack = case stackPopV stack of
  Nothing -> Nothing
  Just (slot1, mem1, stack2) ->
    case stackPopV stack2 of
      Nothing -> Nothing
      Just (slot2, mem2, stack3) -> Just (slot1, mem1, slot2, mem2, stack3)

-- Symbolic Memories
symMemsEmpty :: SymMems
symMemsEmpty = SymMems {smems_list = []}

symMemsAppend :: SMemRef -> SymMems -> SymMems
symMemsAppend mem smems = smems {smems_list = smems_list smems ++ [mem]}

-- State
stateDefault :: State
stateDefault =
  State { st_stack = stackEmpty
        , st_heap = heapEmpty
        , st_base_env_mem = baseMem
        , st_glbl_env_mem = globalMem
        , st_sym_mems = symMemsEmpty
        , st_fresh_count = 1
        , st_pred_unique = 0
        , st_unique = 1 }


