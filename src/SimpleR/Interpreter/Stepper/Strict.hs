module SimpleR.Interpreter.Stepper.Strict
  (
  ) where

import Data.Maybe

import SimpleR.Language
import SimpleR.Interpreter.Commons
import SimpleR.Interpreter.Stepper.ParamArgMatching

-- Helper functions
splitEithers :: [Either a b] -> ([a], [b])
splitEithers [] = ([], [])
splitEithers (e : es) =
  let (as, bs) = splitEithers es in
    case e of
      Left a -> (a : as, bs)
      Right b -> (as, b : bs)

splitBinds ::
  [(Ident, Either MemRef Expr)] -> ([(Ident, MemRef)], [(Ident, Expr)])
splitBinds binds =
  foldl
    (\(accL, accR) (id, rhs) ->
      case rhs of
        Left mem -> (accL ++ [(id, mem)], accR)
        Right expr -> (accL, accR ++ [(id, expr)]))
    ([], []) binds


exprFromArg :: Arg -> Expr
exprFromArg (Arg expr) = expr
exprFromArg (Named _ expr) = expr
exprFromArg (VarArg) = Var idVariadic

padBlankNames :: [Either MemRef (Ident, MemRef)] -> [(String, MemRef)]
padBlankNames [] = []
padBlankNames ((Left mem) : args) = ("", mem) : padBlankNames args
padBlankNames ((Right (id, mem)) : args) = (idName id, mem) : padBlankNames args

liftVariadics ::
  [Either MemRef (Ident, MemRef)] -> MemRef -> Heap -> Maybe (MemRef, Heap)
liftVariadics args envMem heap = do
  let (names, mems) = unzip $ padBlankNames args
  let namesVal = VecVal $ StringVec names
  let (namesMem, heap2) = heapAlloc (DataObj namesVal attrsEmpty) heap
  let varVal = RefsVal mems
  let refsAttrs = attrsInsert "names" namesMem attrsEmpty
  let (refsMem, heap3) = heapAlloc (DataObj varVal refsAttrs) heap2
  heap4 <- heapEnvInsert envMem idVariadic refsMem heap3
  return (refsMem, heap4)


-- Rules

rule_Ident :: State -> [State]
rule_Ident state = maybeToList $ do
  (EvalSlot (Var id), cEnvMem, cStack2) <- stackPopV $ stStack state
  let mem = fromMaybe memNull (heapEnvLookup cEnvMem id (stHeap state))
  let cFrame = mkFrame cEnvMem $ ReturnSlot mem
  return $ state { stStack = stackPush cFrame cStack2 }

rule_MemRef :: State -> [State]
rule_MemRef state = maybeToList $ do
  (EvalSlot (Mem mem), cEnvMem, cStack2) <- stackPopV $ stStack state
  let cFrame = mkFrame cEnvMem $ ReturnSlot mem
  return $ state { stStack = stackPush cFrame cStack2 }

rule_Const :: State -> [State]
rule_Const state = maybeToList $ do
  (EvalSlot (Const const), cEnvMem, cStack2) <- stackPopV $ stStack state
  let (mem, heap2) = heapAllocConst const $ stHeap state
  let cFrame = mkFrame cEnvMem $ ReturnSlot mem
  return $ state { stStack = stackPush cFrame cStack2
                 , stHeap = heap2 }

rule_Seq :: State -> [State]
rule_Seq state = maybeToList $ do
  (EvalSlot (Seq exprs), cEnvMem, cStack2) <- stackPopV $ stStack state
  let cFrames = map (mkFrame cEnvMem . EvalSlot) exprs
  return $ state { stStack = stackPushList cFrames cStack2 }

rule_LambdaAbs :: State -> [State]
rule_LambdaAbs state = maybeToList $ do
  (EvalSlot (LambdaAbs params expr), cEnvMem, cStack2)
    <- stackPopV $ stStack state
  let fEnv = envAssignPred cEnvMem envEmpty
  let fEnvObj = DataObj (EnvVal fEnv) attrsEmpty
  let (fEnvMem, heap2) = heapAlloc fEnvObj $ stHeap state
  let funObj = DataObj (FunVal fEnvMem params expr) attrsEmpty
  let (funMem, heap3) = heapAlloc funObj heap2
  let cFrame = mkFrame cEnvMem $ ReturnSlot funMem
  return $ state { stHeap = heap3
                 , stStack = stackPush cFrame cStack2 }

rule_LambdaAppFun :: State -> [State]
rule_LambdaAppFun state = maybeToList $ do
  (EvalSlot (LambdaApp fun args), cEnvMem, cStack2)
    <- stackPopV $ stStack state
  let fFrame = mkFrame cEnvMem $ EvalSlot fun
  let cFrame = mkFrame cEnvMem $ LambdaASlot Nothing [] Nothing args
  return $ state { stStack = stackPushList [fFrame, cFrame] cStack2 }

rule_LambdaAppFunRet :: State -> [State]
rule_LambdaAppFunRet state = maybeToList $ do
  (ReturnSlot funMem, _,
   LambdaASlot Nothing [] Nothing args, cEnvMem,
   cStack2) <- stackPopV2 $ stStack state
  let lamASlot = LambdaASlot (Just funMem) [] Nothing args
  let cFrame = mkFrame cEnvMem lamASlot
  return $ state { stStack = stackPush cFrame cStack2 }

rule_LambdaAppArg :: State -> [State]
rule_LambdaAppArg state = maybeToList $ do
  (LambdaASlot (Just funMem) dones Nothing (arg : args),
   cEnvMem, cStack2) <- stackPopV $ stStack state
  let aFrame = mkFrame cEnvMem $ EvalSlot $ exprFromArg arg
  let lamASlot = LambdaASlot (Just funMem) dones (Just arg) args
  let cFrame = mkFrame cEnvMem lamASlot
  return $ state { stStack = stackPushList [aFrame, cFrame] cStack2}

rule_LambdaAppArgRet :: State -> [State]
rule_LambdaAppArgRet state = maybeToList $ do
  (ReturnSlot argMem, _,
   LambdaASlot (Just funMem) dones (Just arg) args, cEnvMem,
   cStack2) <- stackPopV2 $ stStack state
  let dones2 = dones ++ [(arg, argMem)]
  let lamASlot = LambdaASlot (Just funMem) dones2 Nothing args
  let cFrame = mkFrame cEnvMem lamASlot
  return $ state { stStack = stackPush cFrame cStack2 }

rule_LambdaAppEnter :: State -> [State]
rule_LambdaAppEnter state = maybeToList $ do
  (LambdaASlot (Just funMem) dones Nothing [], cEnvMem, cStack2)
    <- stackPopV $ stStack state
  -- Get out the things in the heap
  DataObj (FunVal fEnvMem params body) _ <- heapLookup funMem $ stHeap state
  DataObj (EnvVal cEnv) _ <- heapLookup cEnvMem $ stHeap state
  (binds, vars) <- matchLambdaApp params dones cEnv $ stHeap state
  -- Bind the variadic sand memBinds first
  let (memBinds, exprBinds) = splitBinds binds
  (_, heap2) <- liftVariadics vars fEnvMem $ stHeap state
  heap3 <- heapEnvInsertList fEnvMem memBinds heap2
  -- Now add the expression bindings before the body thing
  let aFrames = map (\(id, expr) ->
                 mkFrame fEnvMem $ EvalSlot $ Assign (Var id) expr) exprBinds
  let fFrame = mkFrame fEnvMem $ EvalSlot body
  let cFrame = mkFrame cEnvMem $ LambdaBSlot funMem
  return $
    state { stStack = stackPushList (aFrames ++ [fFrame, cFrame]) cStack2
          , stHeap = heap3 }



