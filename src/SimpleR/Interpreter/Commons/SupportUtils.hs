module SimpleR.Interpreter.Commons.SupportUtils where

import qualified Data.Map as M
import qualified Data.Set as S

import Debug.Trace

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

-- MemAddr
memFromInt :: Int -> MemAddr
memFromInt int = MemAddr { memAddr = int }

memNull :: MemAddr
memNull = memFromInt 0

memNext :: MemAddr -> MemAddr
memNext mem = memFromInt $ 1 + memAddr mem

baseMem :: MemAddr
baseMem = memNull

globalMem :: MemAddr
globalMem = memNull

-- Identifier
idVariadic :: Ident
idVariadic = idFromString "..."

idNull :: Ident
idNull = idFromString "NULL"

idAttrsNames :: Ident
idAttrsNames = idFromString "names"

idFresh :: State -> (Ident, State)
idFresh state = idFreshSeeded "_" state

idFreshSeeded :: String -> State -> (Ident, State)
idFreshSeeded seed state =
  let ct = stFreshCount state in
    (idFromString $ seed ++ "#" ++ show ct, state { stFreshCount = ct + 1})

-- Environment
envEmpty :: Env
envEmpty = Env { envMap = M.empty, envPredMem = globalMem }

envLookup :: Ident -> Env -> Maybe MemAddr
envLookup id env = M.lookup id $ envMap env

envInsert :: Ident -> MemAddr -> Env -> Env
envInsert id mem env = envInsertList [(id, mem)] env

envInsertList :: [(Ident, MemAddr)] -> Env -> Env
envInsertList kvs env = env { envMap = mapInsertList kvs $ envMap env }

envDelete :: Ident -> Env -> Env
envDelete id env = envDeleteList [id] env

envDeleteList :: [Ident] -> Env -> Env
envDeleteList ids env = env { envMap = mapDeleteList ids $ envMap env }

envAssignPred :: MemAddr -> Env -> Env
envAssignPred mem env = env { envPredMem = mem }

envBinds :: Env -> [(Ident, MemAddr)]
envBinds env = M.toList $ envMap env

-- Heap
heapEmpty :: Heap
heapEmpty = Heap { heapMap = M.empty, heapNextMem = memNext memNull }

heapLookup :: MemAddr -> Heap -> Maybe HeapObj
heapLookup mem heap = M.lookup mem $ heapMap heap

heapEnvLookup :: MemAddr -> Ident -> Heap -> Maybe MemAddr
heapEnvLookup envMem id heap
  | Just (DataObj (EnvVal env) _) <- heapLookup envMem heap =
      envLookup id env
  | otherwise = Nothing

heapEnvLookupDeep :: MemAddr -> Ident -> Heap -> Maybe MemAddr
heapEnvLookupDeep envMem id heap
  | Just (DataObj (EnvVal env) _) <- heapLookup envMem heap =
      case envLookup id env of
        Just mem -> Just mem
        _ -> heapEnvLookupDeep (envPredMem env) id heap

  | otherwise = Nothing

heapEnvLookupDeepFun :: MemAddr -> Ident -> Heap -> Maybe MemAddr
heapEnvLookupDeepFun envMem id heap
  | Just (DataObj (EnvVal env) _) <- heapLookup envMem heap =
      case envLookup id env of
        Just mem ->
          case heapLookup mem heap of
            Just (DataObj (FunVal _ _ _) _) -> Just mem
            _ -> heapEnvLookupDeepFun (envPredMem env) id heap
        _ -> heapEnvLookupDeepFun (envPredMem env) id heap

  | otherwise = Nothing
  
heapInsert :: MemAddr -> HeapObj -> Heap -> Heap
heapInsert mem hobj heap = heapInsertList [(mem, hobj)] heap

heapInsertList :: [(MemAddr, HeapObj)] -> Heap -> Heap
heapInsertList kvs heap = heap { heapMap = mapInsertList kvs $ heapMap heap }

heapDelete :: MemAddr -> Heap -> Heap
heapDelete mem heap = heapDeleteList [mem] heap

heapDeleteList :: [MemAddr] -> Heap -> Heap
heapDeleteList mems heap =
  heap { heapMap = mapDeleteList mems $ heapMap heap }

heapAlloc :: HeapObj -> Heap -> (MemAddr, Heap)
heapAlloc hobj heap =
  let usedMem = heapNextMem heap in
  let heap2 = heapInsert usedMem hobj heap in
    (usedMem, heap2 { heapNextMem = memNext usedMem })

heapAllocList :: [HeapObj] -> Heap -> ([MemAddr], Heap)
heapAllocList [] heap = ([], heap)
heapAllocList (hobj : hobjs) heap =
  let (usedMem, heap2) = heapAlloc hobj heap in
  let (usedMems, heap3) = heapAllocList hobjs heap2 in
    (usedMem : usedMems, heap3)

heapAllocConst :: Const -> Heap -> (MemAddr, Heap)
heapAllocConst const heap =
  heapAlloc (DataObj (VecVal (vecFromConst const)) attrsEmpty) heap

heapBinds :: Heap -> [(MemAddr, HeapObj)]
heapBinds heap = M.toList $ heapMap heap

heapEnvInsert :: Ident -> MemAddr -> MemAddr -> Heap -> Maybe Heap
heapEnvInsert id mem envMem heap = heapEnvInsertList [(id, mem)] envMem heap

heapEnvInsertList :: [(Ident, MemAddr)] -> MemAddr -> Heap -> Maybe Heap
heapEnvInsertList kvs envMem heap
  | Just (DataObj (EnvVal env) attrs) <- heapLookup envMem heap =
      let env2 = envInsertList kvs env in
        Just $ heapInsert envMem (DataObj (EnvVal env2) attrs) heap
  | otherwise = Nothing

heapCopy :: MemAddr -> Heap -> Maybe (MemAddr, Heap)
heapCopy mem heap
  | Just (DataObj val attrs) <- heapLookup mem heap = do
      (kvs2, heap2) <- heapCopyKeyMems (attrsBinds attrs) heap
      (val2, heap3) <- heapCopyValue val heap2
      let attrs2 = attrsInsertList kvs2 attrsEmpty
      return $ heapAlloc (DataObj val2 attrs2) heap3
  | otherwise = Nothing

heapCopyKeyMems ::
  (Ord k) => [(k, MemAddr)] -> Heap -> Maybe ([(k, MemAddr)], Heap)
heapCopyKeyMems [] heap = Just ([], heap)
heapCopyKeyMems ((key, mem) : keyMems) heap = do
  (mem2, heap2) <- heapCopy mem heap
  (keyMems2, heap3) <- heapCopyKeyMems keyMems heap2
  return $ ((key, mem2) : keyMems2, heap3)

heapCopyValue :: Value -> Heap -> Maybe (Value, Heap)
heapCopyValue (VecVal vec) heap = Just $ (VecVal vec, heap)
heapCopyValue (FunVal envMem params body) heap =
  Just $ (FunVal envMem params body, heap)
heapCopyValue (EnvVal env) heap = do
  (kvs, heap2) <- heapCopyKeyMems (envBinds env) heap
  let env2 = envAssignPred (envPredMem env) $ envInsertList kvs envEmpty
  return (EnvVal env2, heap2)

-- Attributes
attrsEmpty :: Attributes
attrsEmpty = Attributes { attrsMap = M.empty }

attrsLookup :: Ident -> Attributes -> Maybe MemAddr
attrsLookup id attrs = M.lookup id $ attrsMap attrs

attrsInsert :: Ident -> MemAddr -> Attributes -> Attributes
attrsInsert id mem attrs = attrsInsertList [(id, mem)] attrs

attrsInsertList :: [(Ident, MemAddr)] -> Attributes -> Attributes
attrsInsertList kvs attrs =
  attrs { attrsMap = mapInsertList kvs $ attrsMap attrs }

attrsDelete :: Ident -> Attributes -> Attributes
attrsDelete id attrs = attrsDeleteList [id] attrs

attrsDeleteList :: [Ident] -> Attributes -> Attributes
attrsDeleteList ids attrs =
  attrs { attrsMap = mapDeleteList ids $ attrsMap attrs }

attrsBinds :: Attributes -> [(Ident, MemAddr)]
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

stackPopV :: Stack -> Maybe (Cont, MemAddr, Stack)
stackPopV stack = do
  (frame, stack2) <- stackPop stack
  return (frameCont frame, frameEnvMem frame, stack2)

stackPopV2 :: Stack -> Maybe (Cont, MemAddr, Cont, MemAddr, Stack)
stackPopV2 stack = do
  (cont1, envMem1, stack2) <- stackPopV stack
  (cont2, envMem2, stack3) <- stackPopV stack2
  return (cont1, envMem1, cont2, envMem2, stack3)

-- Frames
frameMk :: MemAddr -> Cont -> Frame
frameMk envMem cont =
  Frame { frameEnvMem = envMem, frameCont = cont }

-- Constraint
constrEmpty :: Constraint
constrEmpty =
  Constraint { constrAssertList = [], constrPreList = [], constrPostList = [] }

constrAssertsAppend :: [SmtExpr] -> Constraint -> Constraint
constrAssertsAppend smts constr =
  constr { constrAssertList = (constrAssertList constr) ++ smts }

constrPresAppend :: [SmtCmd] -> Constraint -> Constraint
constrPresAppend smts constr =
  constr { constrPreList = (constrPreList constr) ++ smts }

constrPostsAppend :: [SmtCmd] -> Constraint -> Constraint
constrPostsAppend smts constr =
  constr { constrPostList = (constrPostList constr) ++ smts }

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


