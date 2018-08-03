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

-- Extract default (Ident, Expr) bindings from a [Param]
bindsOfDefaults :: [Param] -> [(Ident, Expr)]
bindsOfDefaults [] = []
bindsOfDefaults ((Param _) : ps) = bindsOfDefaults ps
bindsOfDefaults (VarParam : ps) = bindsOfDefaults ps
bindsOfDefaults ((Default id expr) : ps) = (id, expr) : bindsOfDefaults ps

-- Linearize the arguments into a [Either MemRef (Ident, MemRef)] if possible.
-- Flattens the variadics.
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
        let strMemPairs = zip nameStrs varMems in
        let idMemPairs = map (\(n, m) -> if n == ""
                           then Left m else Right (mkId n, m)) strMemPairs in
          Just $ idMemPairs ++ args2
      else
        Nothing

-- Remove the param that is used based on the Ident.
dropUsedParam :: Ident -> [Param] -> [Param]
dropUsedParam id params =
  filter
    (\p -> case p of
      Param pid -> id /= pid
      Default pid _ -> id /= pid
      VarParam -> True)
    params

-- Intended as the function for foldl over matching the [Param] to the
-- pulled arguments; quite, hacky, yes!
-- The idea is that we go through a list of arguments, and gradually
-- filter out the [Param] in the first element of the accumulator triple,
-- which is then left to be positionally matched with
-- the second element, consisting of [Either MemRef (Ident, MemRef)]
-- The accumulator contains a triple of
--   [Param] -- the parameters still unmatched
--   [Either MemRef (Ident, MemRef)] -- args to be positionally matched
--   [(Ident, MemRef)] -- The default argument pairings
defaultMatchFoldLFun ::
  ([Param], [Either MemRef (Ident, MemRef)], [(Ident, MemRef)]) ->
  (Either MemRef (Ident, MemRef)) ->
    ([Param], [Either MemRef (Ident, MemRef)], [(Ident, MemRef)])
defaultMatchFoldLFun (params, posArgs, namedArgs) arg =
  case arg of
    Left mem -> (params, posArgs ++ [arg], namedArgs)
    Right (id, mem) ->
      let filtParams = dropUsedParam id params in
        -- Managed to have named binding match
        if length filtParams < length params then
          (filtParams, posArgs, namedArgs ++ [(id, mem)])
        -- Unsuccessful and accomplished nothing
        else
          (filtParams, posArgs ++ [arg], namedArgs)


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




