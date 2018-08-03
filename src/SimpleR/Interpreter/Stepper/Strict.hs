module SimpleR.Interpreter.Stepper.Strict
  (
  ) where

import SimpleR.Language
import SimpleR.Interpreter.Commons

splitEithers :: [Either a b] -> ([a], [b])
splitEithers [] = ([], [])
splitEithers (e : es) =
  let (as, bs) = splitEithers es in
    case e of
      Left a -> (a : as, bs)
      Right b -> (as, b : bs)

bindsOfDefaults :: [Param] -> [(Ident, Expr)]
bindsOfDefaults [] = []
bindsOfDefaults ((Param _) : ps) = bindsOfDefaults ps
bindsOfDefaults (VarParam : ps) = bindsOfDefaults ps
bindsOfDefaults ((Default id expr) : ps) = (id, expr) : bindsOfDefaults ps

pullArgs :: [(Arg, MemRef)] -> Heap -> Maybe [Either MemRef (Ident, MemRef)]
pullArgs [] _ = Just []
pullArgs (arg : args) heap = do
  args2 <- pullArgs args heap
  case arg of
    (Arg _, mem) -> Just $ (Left mem) : args2
    (Named id expr, mem) -> Just $ (Right (id, mem)) : args2
    (VarArg, mem) -> do
      (DataObj (RefsVal varMems) attrs) <- heapLookup mem heap
      nameMem <- attrsLookup "name" attrs
      (DataObj (VecVal (StringVec nameStrs)) _) <- heapLookup nameMem heap
      if length nameStrs == length varMems then
        let pairs = map Right $ zip (map mkId nameStrs) varMems in
          Just $ pairs ++ args2
      else
        Nothing




rule_Ident :: State -> [State]
rule_Ident state =
  case stackPopV $ stStack state of
    Just (EvalSlot (Var id), cEnvMem, cStack2) ->
      case heapEnvLookup cEnvMem id (stHeap state) of
        Just mem ->
          let cFrame = mkFrame cEnvMem (ReturnSlot mem) in
            [state { stStack = stackPush cFrame cStack2 }]
        Nothing ->
          let cFrame = mkFrame cEnvMem (ReturnSlot memNull) in
            [state { stStack = stackPush cFrame cStack2 }]
    _ -> []

rule_MemRef :: State -> [State]
rule_MemRef state =
  case stackPopV $ stStack state of
    Just (EvalSlot (Mem mem), cEnvMem, cStack2) ->
      let cFrame = mkFrame cEnvMem (ReturnSlot mem) in
        [state { stStack = stackPush cFrame cStack2 }]
    _ -> []

rule_Const :: State -> [State]
rule_Const state =
  case stackPopV $ stStack state of
    Just (EvalSlot (Const const), cEnvMem, cStack2) ->
      let (mem, heap2) = heapAllocConst const $ stHeap state in
      let cFrame = mkFrame cEnvMem (ReturnSlot mem) in
       [state { stStack = stackPush cFrame cStack2
              , stHeap = heap2 }]
    _ -> []

rule_Seq :: State -> [State]
rule_Seq state =
  case stackPopV $ stStack state of
    Just (EvalSlot (Seq exprs), cEnvMem, cStack2) ->
      if length exprs == 0 then
        let cFrame = mkFrame cEnvMem (ReturnSlot memNull) in
          [state { stStack = stackPush cFrame cStack2 }]
      else
        let cFrames = map (mkFrame cEnvMem . EvalSlot) exprs in
          [state { stStack = stackPushList cFrames cStack2 }]
    _ -> []




