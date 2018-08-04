module SimpleR.Interpreter.Commons.SupportUtils where

import qualified Data.Map as M

import SimpleR.Language
import SimpleR.Interpreter.Commons.Support
import SimpleR.Smt

mapInsertList :: (Ord k) => [(k, v)] -> M.Map k v -> M.Map k v
mapInsertList kvs map = foldl (\m (k, v) -> M.insert k v m) map kvs

mapDeleteList :: (Ord k) => [k] -> M.Map k v -> M.Map k v
mapDeleteList ks map = foldl (\m k -> M.delete k m) map ks

-- Memories
baseMem :: MemRef
baseMem = memNull

globalMem :: MemRef
globalMem = memNull

-- Identifier
idVariadic :: Ident
idVariadic = mkId "..."

mkIdFresh :: State -> (Ident, State)
mkIdFresh = undefined

mkIdFreshSeeded :: String -> State -> (Ident, State)
mkIdFreshSeeded = undefined

-- Vector conversion
mkConstVec :: Const -> Vector
mkConstVec (IntConst int) = IntVec [int]
mkConstVec (DoubleConst double) = DoubleVec [double]
mkConstVec (ComplexConst complex) = ComplexVec [complex]
mkConstVec (BoolConst bool) = BoolVec [bool]
mkConstVec (StringConst str) = StringVec [str]
mkConstVec (NaConst) = NilVec

-- Environment
envEmpty :: Env
envEmpty = Env { envMap = M.empty, envPredMem = globalMem }

envLookup :: Ident -> Env -> Maybe MemRef
envLookup id env = M.lookup id $ envMap env

envInsert :: Ident -> MemRef -> Env -> Env
envInsert id mem env = envInsertList [(id, mem)] env

envInsertList :: [(Ident, MemRef)] -> Env -> Env
envInsertList kvs env = env { envMap = mapInsertList kvs $ envMap env }

envDelete :: Ident -> Env -> Env
envDelete id env = envDeleteList [id] env

envDeleteList :: [Ident] -> Env -> Env
envDeleteList ids env = env { envMap = mapDeleteList ids $ envMap env }

envAssignPred :: MemRef -> Env -> Env
envAssignPred mem env = env { envPredMem = mem }

envBinds :: Env -> [(Ident, MemRef)]
envBinds env = M.toList $ envMap env

-- Heap
heapEmpty :: Heap
heapEmpty = Heap { heapMap = M.empty, heapNextMem = memNext memNull }

heapLookup :: MemRef -> Heap -> Maybe HeapObj
heapLookup mem heap = M.lookup mem $ heapMap heap

heapEnvLookup :: MemRef -> Ident -> Heap -> Maybe MemRef
heapEnvLookup envMem id heap = do
  DataObj (EnvVal env) _ <- heapLookup envMem heap
  envLookup id env

heapInsert :: MemRef -> HeapObj -> Heap -> Heap
heapInsert mem hobj heap = heapInsertList [(mem, hobj)] heap

heapInsertList :: [(MemRef, HeapObj)] -> Heap -> Heap
heapInsertList kvs heap = heap { heapMap = mapInsertList kvs $ heapMap heap }

heapDelete :: MemRef -> Heap -> Heap
heapDelete mem heap = heapDeleteList [mem] heap

heapDeleteList :: [MemRef] -> Heap -> Heap
heapDeleteList mems heap =
  heap { heapMap = mapDeleteList mems $ heapMap heap }

heapAlloc :: HeapObj -> Heap -> (MemRef, Heap)
heapAlloc hobj heap =
  let usedMem = heapNextMem heap in
  let heap2 = heapInsert usedMem hobj heap in
    (usedMem, heap2 { heapNextMem = memNext usedMem })

heapAllocList :: [HeapObj] -> Heap -> ([MemRef], Heap)
heapAllocList [] heap = ([], heap)
heapAllocList (hobj : hobjs) heap =
  let (usedMem, heap2) = heapAlloc hobj heap in
  let (usedMems, heap3) = heapAllocList hobjs heap2 in
    (usedMem : usedMems, heap3)

heapAllocConst :: Const -> Heap -> (MemRef, Heap)
heapAllocConst const heap =
  heapAlloc (DataObj (VecVal (mkConstVec const)) attrsEmpty) heap

heapBinds :: Heap -> [(MemRef, HeapObj)]
heapBinds heap = M.toList $ heapMap heap

heapEnvInsert :: MemRef -> Ident -> MemRef -> Heap -> Maybe Heap
heapEnvInsert envMem id mem heap = heapEnvInsertList envMem [(id, mem)] heap

heapEnvInsertList :: MemRef -> [(Ident, MemRef)] -> Heap -> Maybe Heap
heapEnvInsertList envMem kvs heap = do
  DataObj (EnvVal env) attrs <- heapLookup envMem heap
  let env2 = envInsertList kvs env
  return $ heapInsert envMem (DataObj (EnvVal env2) attrs) heap

-- Attributes
attrsEmpty :: Attributes
attrsEmpty = Attributes { attrsMap = M.empty }

attrsLookup :: String -> Attributes -> Maybe MemRef
attrsLookup str attrs = M.lookup str $ attrsMap attrs

attrsInsert :: String -> MemRef -> Attributes -> Attributes
attrsInsert str mem attrs = attrsInsertList [(str, mem)] attrs

attrsInsertList :: [(String, MemRef)] -> Attributes -> Attributes
attrsInsertList kvs attrs =
  attrs { attrsMap = mapInsertList kvs $ attrsMap attrs }

attrsDelete :: String -> Attributes -> Attributes
attrsDelete str attrs = attrsDeleteList [str] attrs

attrsDeleteList :: [String] -> Attributes -> Attributes
attrsDeleteList strs attrs =
  attrs { attrsMap = mapDeleteList strs $ attrsMap attrs }

attrsBinds :: Attributes -> [(String, MemRef)]
attrsBinds attrs = M.toList $ attrsMap attrs

-- Stack
stackEmpty :: Stack
stackEmpty = Stack { stackList = [] }

stackPush :: Frame -> Stack -> Stack
stackPush frame stack = stackPushList [frame] stack

stackPushList :: [Frame] -> Stack -> Stack
stackPushList frames stack = stack { stackList = frames ++ stackList stack }

stackPop :: Stack -> Maybe (Frame, Stack)
stackPop stack =
  case stackList stack of
    [] -> Nothing
    (frame : frames) -> return (frame, stack { stackList = frames })

stackPopV :: Stack -> Maybe (Slot, MemRef, Stack)
stackPopV stack = do
  (frame, stack2) <- stackPop stack
  return (frameSlot frame, frameEnvMem frame, stack2)

stackPopV2 :: Stack -> Maybe (Slot, MemRef, Slot, MemRef, Stack)
stackPopV2 stack = do
  (slot1, envMem1, stack2) <- stackPopV stack
  (slot2, envMem2, stack3) <- stackPopV stack2
  return (slot1, envMem1, slot2, envMem2, stack3)

-- Frames
frameDefault :: Frame
frameDefault = Frame { frameEnvMem = memNull, frameSlot = ReturnSlot memNull }

mkFrame :: MemRef -> Slot -> Frame
mkFrame envMem slot = frameDefault { frameEnvMem = envMem, frameSlot = slot }

-- Symbolic Memories
symMemsEmpty :: SymMems
symMemsEmpty = SymMems { symMemsList = [] }

symMemsAppend :: MemRef -> SymMems -> SymMems
symMemsAppend mem smems = smems { symMemsList = symMemsList smems ++ [mem] }

-- Constraint
constrEmpty :: Constraint
constrEmpty = Constraint { constrList = [] }

constrAppend :: SmtExpr -> Constraint -> Constraint
constrAppend smt constr = constrAppendList [smt] constr

constrAppendList :: [SmtExpr] -> Constraint -> Constraint
constrAppendList smts constr =
  constr { constrList = (constrList constr) ++ smts }

-- State
stateDefault :: State
stateDefault =
  State { stStack = stackEmpty
        , stHeap = heapEmpty
        , stBaseEnvMem = baseMem
        , stGlobalEnvMem = globalMem
        , stSymMems = symMemsEmpty
        , stConstr = constrEmpty
        , stFreshCount = 1
        , stPredUnique = 0
        , stUnique = 1 }


