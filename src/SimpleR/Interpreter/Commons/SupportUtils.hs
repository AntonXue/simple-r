module SimpleR.Interpreter.Commons.SupportUtils where

import qualified Data.Map as M
import qualified Data.Set as S

import SimpleR.Language
import SimpleR.Interpreter.Commons.Support
import SimpleR.Interpreter.Commons.Vector
import SimpleR.Smt

mapInsertList :: (Ord k) => [(k, v)] -> M.Map k v -> M.Map k v
mapInsertList kvs map = foldl (\m (k, v) -> M.insert k v m) map kvs

mapDeleteList :: (Ord k) => [k] -> M.Map k v -> M.Map k v
mapDeleteList ks map = foldl (\m k -> M.delete k m) map ks

setInsertList :: (Ord a) => [a] -> S.Set a -> S.Set a
setInsertList as set = foldl (\s a -> S.insert a s) set as

-- MemRef
memFromInt :: Int -> MemRef
memFromInt int = MemRef { memAddr = int }

memNull :: MemRef
memNull = memFromInt 0

memNext :: MemRef -> MemRef
memNext mem = memFromInt $ 1 + memAddr mem

baseMem :: MemRef
baseMem = memNull

globalMem :: MemRef
globalMem = memNull

-- Identifier
idVariadic :: Ident
idVariadic = idFromString "..."

idNull :: Ident
idNull = idFromString "NULL"

idFresh :: State -> (Ident, State)
idFresh = undefined

idFreshSeeded :: String -> State -> (Ident, State)
idFreshSeeded = undefined

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
heapEnvLookup envMem id heap
  | Just (DataObj (EnvVal env) _) <- heapLookup envMem heap =
      envLookup id env
  | otherwise = Nothing

heapEnvLookupDeep :: MemRef -> Ident -> Heap -> Maybe MemRef
heapEnvLookupDeep envMem id heap
  | Just (DataObj (EnvVal env) _) <- heapLookup envMem heap
  , Just mem <- envLookup id env = Just mem

  | Just (DataObj (EnvVal env) _) <- heapLookup envMem heap
  , Nothing <- envLookup id env =
      heapEnvLookup (envPredMem env) id heap

  | otherwise = Nothing

heapEnvLookupDeepFun :: MemRef -> Ident -> Heap -> Maybe MemRef
heapEnvLookupDeepFun envMem id heap
  | Just (DataObj (EnvVal env) _) <- heapLookup envMem heap
  , Just mem <- envLookup id env
  , Just (DataObj val _) <- heapLookup mem heap =
      case val of
        FunVal _ _ _ -> Just mem
        _ -> heapEnvLookupDeepFun (envPredMem env) id heap

  | otherwise = Nothing
  

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
  heapAlloc (DataObj (VecVal (vecFromConst const)) attrsEmpty) heap

heapBinds :: Heap -> [(MemRef, HeapObj)]
heapBinds heap = M.toList $ heapMap heap

heapEnvInsert :: Ident -> MemRef -> MemRef -> Heap -> Maybe Heap
heapEnvInsert id mem envMem heap = heapEnvInsertList [(id, mem)] envMem heap

heapEnvInsertList :: [(Ident, MemRef)] -> MemRef -> Heap -> Maybe Heap
heapEnvInsertList kvs envMem heap
  | Just (DataObj (EnvVal env) attrs) <- heapLookup envMem heap =
      let env2 = envInsertList kvs env in
        Just $ heapInsert envMem (DataObj (EnvVal env2) attrs) heap
  | otherwise = Nothing

heapCopy :: MemRef -> Heap -> Maybe (MemRef, Heap)
heapCopy mem heap = undefined

heapCopyKeyMems ::
  (Ord k) => [(k, MemRef)] -> Heap -> Maybe ([(k, MemRef)], Heap)
heapCopyKeyMems [] heap = Just ([], heap)
heapCopyKeyMems ((key, mem) : keyMems) heap = do
  (mem2, heap2) <- heapCopy mem heap
  (keyMems2, heap3) <- heapCopyKeyMems keyMems heap2
  return $ ((key, mem2) : keyMems2, heap3)

heapCopyValue :: Value -> Heap -> Maybe (Value, Heap)
heapCopyValue = undefined

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
    (frame : frames) -> Just (frame, stack { stackList = frames })

stackPopV :: Stack -> Maybe (Cont, MemRef, Stack)
stackPopV stack = do
  (frame, stack2) <- stackPop stack
  return (frameCont frame, frameEnvMem frame, stack2)

stackPopV2 :: Stack -> Maybe (Cont, MemRef, Cont, MemRef, Stack)
stackPopV2 stack = do
  (cont1, envMem1, stack2) <- stackPopV stack
  (cont2, envMem2, stack3) <- stackPopV stack2
  return (cont1, envMem1, cont2, envMem2, stack3)

-- Frames
frameMk :: MemRef -> Cont -> Frame
frameMk envMem cont =
  Frame { frameEnvMem = envMem, frameCont = cont }

-- Constraint
constrEmpty :: Constraint
constrEmpty = Constraint { constrList = [] }

constrAppend :: SmtExpr -> Constraint -> Constraint
constrAppend smt constr = constrAppendList [smt] constr

constrAppendList :: [SmtExpr] -> Constraint -> Constraint
constrAppendList smts constr =
  constr { constrList = (constrList constr) ++ smts }

-- Pures
puresEmpty :: Pures
puresEmpty = Pures { puresSet = S.empty }

puresInsert :: Ident -> Pures -> Pures
puresInsert id pures = puresInsertList [id] pures

puresInsertList :: [Ident] -> Pures -> Pures
puresInsertList ids pures =
  Pures { puresSet = setInsertList ids $ puresSet pures }

puresMember :: Ident -> Pures -> Bool
puresMember id pures = S.member id $ puresSet pures

-- State
stateDefault :: State
stateDefault =
  State { stRedex = ResultRed memNull
        , stStack = stackEmpty
        , stHeap = heapEmpty
        , stBaseEnvMem = baseMem
        , stGlobalEnvMem = globalMem
        , stPures = puresEmpty
        , stConstr = constrEmpty
        , stFreshCount = 1
        , stPredUnique = 0
        , stUnique = 1 }


