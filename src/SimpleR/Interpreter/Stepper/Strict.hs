module SimpleR.Interpreter.Stepper.Strict
  ( Rule(..)
  , rulePairs
  ) where

import Data.Maybe

import SimpleR.Language
import SimpleR.Interpreter.Commons
import SimpleR.Interpreter.Stepper.ParamArgMatching
import SimpleR.Interpreter.Natives

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

isMemConcTrue :: MemRef -> Heap -> Bool
isMemConcTrue mem heap =
  case heapLookup mem heap of
    Just (DataObj (VecVal vec) _) ->
      case vec of
        IntVec (x : _) -> x /= 0
        DoubleVec (x : _) -> x /= 0
        ComplexVec (x : _) -> x /= 0
        BoolVec (x : _) -> x
        _ -> False
    _ -> False

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

unwindToLoopSlot :: Stack -> Maybe ((Expr, Expr, LoopState), MemRef, Stack)
unwindToLoopSlot stack = do
  (slot, lEnvMem, stack2) <- stackPopV stack
  case slot of
    LoopSlot cond body loop -> return ((cond, body, loop), lEnvMem, stack2)
    _ -> unwindToLoopSlot stack2

unwindToLambdaBSlot :: Stack -> Maybe (MemRef, MemRef, Stack)
unwindToLambdaBSlot stack = do
  (slot, lEnvMem, stack2) <- stackPopV stack
  case slot of
    LambdaBSlot funMem -> return (funMem, lEnvMem, stack2)
    _ -> unwindToLambdaBSlot stack2



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

rule_LambdaAppRet :: State -> [State]
rule_LambdaAppRet state = maybeToList $ do
  (ReturnSlot mem, _,
   LambdaBSlot _, cEnvMem,
   cStack2) <- stackPopV2 $ stStack state
  let cFrame = mkFrame cEnvMem $ ReturnSlot mem
  return $ state { stStack = stackPush cFrame cStack2 }

rule_NativeLambdaApp :: State -> [State]
rule_NativeLambdaApp state =
  case stackPopV $ stStack state of
    Just (EvalSlot (NativeLambdaApp _ _), _, _) -> nativeCall state
    _ -> []

rule_AssignId :: State -> [State]
rule_AssignId state = maybeToList $ do
  (EvalSlot (Assign (Var id) expr), cEnvMem, cStack2)
    <- stackPopV $ stStack state
  let eFrame = mkFrame cEnvMem $ EvalSlot expr
  let cFrame = mkFrame cEnvMem $ AssignSlot id
  return $ state { stStack = stackPushList [eFrame, cFrame] cStack2 }

rule_AssignStr :: State -> [State]
rule_AssignStr state = maybeToList $ do
  (EvalSlot (Assign (Const (StringConst str)) expr), cEnvMem, cStack2)
    <- stackPopV $ stStack state
  let cFrame = mkFrame cEnvMem $ EvalSlot $ Assign (Var $ mkId str) expr
  return $ state { stStack = stackPush cFrame cStack2 }

rule_AssignRet :: State -> [State]
rule_AssignRet state = maybeToList $ do
  (ReturnSlot mem, _, AssignSlot id, cEnvMem, cStack2)
    <- stackPopV2 $ stStack state
  heap2 <- heapEnvInsert cEnvMem id mem $ stHeap state
  let cFrame = mkFrame cEnvMem $ ReturnSlot mem
  return $ state { stHeap = heap2
                 , stStack = stackPush cFrame cStack2 }

rule_If :: State -> [State]
rule_If state = maybeToList $ do
  (EvalSlot (If cond true false), cEnvMem, cStack2)
    <- stackPopV $ stStack state
  let aFrame = mkFrame cEnvMem $ EvalSlot cond
  let cFrame = mkFrame cEnvMem $ BranchSlot true false
  return $ state { stStack = stackPushList [aFrame, cFrame] cStack2 }

rule_IfRet :: State -> [State]
rule_IfRet state = maybeToList $ do
  (ReturnSlot condMem, _,
   BranchSlot true false, cEnvMem,
   cStack2) <- stackPopV2 $ stStack state
  let branch = if isMemConcTrue condMem $ stHeap state then true else false
  let cFrame = mkFrame cEnvMem $ EvalSlot branch
  return $ state { stStack = stackPush cFrame cStack2 }

rule_IfRetSym :: State -> [State]
rule_IfRetSym state = undefined

rule_While :: State -> [State]
rule_While state = maybeToList $ do
  (EvalSlot (While cond body), cEnvMem, cStack2) <- stackPopV $ stStack state
  let dFrame = mkFrame cEnvMem $ EvalSlot cond
  let cFrame = mkFrame cEnvMem $ LoopSlot cond body LoopStateCond
  return $ state { stStack = stackPushList [dFrame, cFrame] cStack2 }

rule_WhileTrue :: State -> [State]
rule_WhileTrue state = maybeToList $ do
  (ReturnSlot condMem, _,
   LoopSlot cond body LoopStateCond, cEnvMem,
   cStack2) <- stackPopV2 $ stStack state
  _ <- if isMemConcTrue condMem $ stHeap state then return True else Nothing
  let bFrame = mkFrame cEnvMem $ EvalSlot body
  let cFrame = mkFrame cEnvMem $ LoopSlot cond body LoopStateBody
  return $ state { stStack = stackPushList [bFrame, cFrame] cStack2 }

rule_WhileBodyRet :: State -> [State]
rule_WhileBodyRet state = maybeToList $ do
  (ReturnSlot _, _,
   LoopSlot cond body LoopStateBody, cEnvMem,
   cStack2) <- stackPopV2 $ stStack state
  let dFrame = mkFrame cEnvMem $ EvalSlot cond
  let cFrame = mkFrame cEnvMem $ LoopSlot cond body LoopStateCond
  return $ state { stStack = stackPushList [dFrame, cFrame] cStack2 }

rule_WhileFalse :: State -> [State]
rule_WhileFalse state = maybeToList $ do
  (ReturnSlot condMem, _,
   LoopSlot cond body LoopStateCond, cEnvMem,
   cStack2) <- stackPopV2 $ stStack state
  let cFrame = mkFrame cEnvMem $ ReturnSlot memNull
  return $ state { stStack = stackPush cFrame cStack2 }

rule_WhileSym :: State -> [State]
rule_WhileSym state = undefined

rule_Break :: State -> [State]
rule_Break state = maybeToList $ do
  (EvalSlot Break, _, cStack2) <- stackPopV $ stStack state
  ((cond, body, _), lEnvMem, lStack) <- unwindToLoopSlot cStack2
  let cFrame = mkFrame lEnvMem $ ReturnSlot memNull
  return $ state { stStack = stackPush cFrame cStack2 }

rule_Next :: State -> [State]
rule_Next state = maybeToList $ do
  (EvalSlot Next, _, cStack2) <- stackPopV $ stStack state
  ((cond, body, _), lEnvMem, lStack) <- unwindToLoopSlot cStack2
  let bFrame = mkFrame lEnvMem $ EvalSlot body
  let cFrame = mkFrame lEnvMem $ LoopSlot cond body LoopStateBody
  return $ state { stStack = stackPushList [bFrame, cFrame] cStack2 }

rule_Return :: State -> [State]
rule_Return state = maybeToList $ do
  (EvalSlot (Return expr), cEnvMem, cStack2) <- stackPopV $ stStack state
  (funMem, lEnvMem, fStack) <- unwindToLambdaBSlot cStack2
  let rFrame = mkFrame cEnvMem $ EvalSlot expr
  let cFrame = mkFrame lEnvMem $ LambdaBSlot funMem
  return $ state { stStack = stackPushList [rFrame, cFrame] fStack }

rule_DiscardRetSlot :: State -> [State]
rule_DiscardRetSlot state = maybeToList $ do
  (ReturnSlot _, _, cStack2) <- stackPopV $ stStack state
  case stackPopV cStack2 of
    Just (EvalSlot _, _, _) -> return $ state { stStack = cStack2 }
    Just (ReturnSlot _, _, _) -> return $ state { stStack = cStack2 }
    Just (SeqSlot _, _, _) -> return $ state { stStack = cStack2 }
    _ -> Nothing
  
rule_Blank :: State -> [State]
rule_Blank _ = []

--

data Rule =
    RuleIdent
  | RuleMemRef
  | RuleConst
  | RuleSeq
  | RuleLambdaAbs
  | RuleLambdaApp
  | RuleLambdaAppFun
  | RuleLambdaAppFunRet
  | RuleLambdaAppArg
  | RuleLambdaAppArgRet
  | RuleLambdaAppEnter
  | RuleLambdaAppRet
  | RuleNativeLambdaApp
  | RuleAssignId
  | RuleAssignStr
  | RuleAssignRet
  | RuleIf
  | RuleIfRet
  | RuleIfSym
  | RuleWhile
  | RuleWhileTrue
  | RuleWhileBodyRet
  | RuleWhileFalse
  | RuleWhileSym
  | RuleBreak
  | RuleNext
  | RuleReturn
  | RuleDiscardRetSlot
  | RuleBlank
  deriving (Ord, Eq, Show, Read)

rulePairs :: [(Rule, State -> [State])]
rulePairs =
  [ (RuleIdent, rule_Ident)
  , (RuleMemRef, rule_MemRef)
  , (RuleConst, rule_Const)
  , (RuleSeq, rule_Seq)
  , (RuleLambdaAbs, rule_LambdaAbs)
  , (RuleLambdaAppFun, rule_LambdaAppFun)
  , (RuleLambdaAppFunRet, rule_LambdaAppFunRet)
  , (RuleLambdaAppArg, rule_LambdaAppArg)
  , (RuleLambdaAppArgRet, rule_LambdaAppArgRet)
  , (RuleLambdaAppEnter, rule_LambdaAppEnter)
  , (RuleLambdaAppRet, rule_LambdaAppRet)
  , (RuleNativeLambdaApp, rule_NativeLambdaApp)
  , (RuleAssignId, rule_AssignId)
  , (RuleAssignStr, rule_AssignStr)
  , (RuleAssignRet, rule_AssignRet)
  , (RuleIf, rule_If)
  , (RuleIfRet, rule_IfRet)
  , (RuleIfSym, rule_IfRetSym)
  , (RuleWhile, rule_While)
  , (RuleWhileTrue, rule_WhileTrue)
  , (RuleWhileBodyRet, rule_WhileBodyRet)
  , (RuleWhileFalse, rule_WhileFalse)
  , (RuleWhileSym, rule_WhileSym)
  , (RuleBreak, rule_Break)
  , (RuleNext, rule_Next)
  , (RuleReturn, rule_Return)
  , (RuleDiscardRetSlot, rule_DiscardRetSlot)
  , (RuleBlank, rule_Blank)]

