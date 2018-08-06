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
  heap4 <- heapEnvInsert idVariadic refsMem envMem heap3
  return (refsMem, heap4)

unwindToLoopSlot :: Stack -> Maybe ((Expr, Expr, LoopState), MemRef, Stack)
unwindToLoopSlot stack = do
  (slot, lEnvMem, stack2) <- stackPopV stack
  case slot of
    LoopSlot cond body loop -> return ((cond, body, loop), lEnvMem, stack2)
    _ -> unwindToLoopSlot stack2

unwindToLamBSlot :: Stack -> Maybe (MemRef, MemRef, Stack)
unwindToLamBSlot stack = do
  (slot, lEnvMem, stack2) <- stackPopV stack
  case slot of
    LamBSlot funMem -> return (funMem, lEnvMem, stack2)
    _ -> unwindToLamBSlot stack2



-- Rules

rule_Ident :: State -> [State]
rule_Ident state = maybeToList $ do
  (EvalSlot (Var id), cEnvMem, cStack2) <- stackPopV $ stStack state
  let mem = fromMaybe memNull (heapEnvLookupDeep cEnvMem id (stHeap state))
  let cFrame = frameMk cEnvMem $ ReturnSlot mem
  return $ state { stStack = stackPush cFrame cStack2 }

rule_MemRef :: State -> [State]
rule_MemRef state = maybeToList $ do
  (EvalSlot (Mem mem), cEnvMem, cStack2) <- stackPopV $ stStack state
  let cFrame = frameMk cEnvMem $ ReturnSlot mem
  return $ state { stStack = stackPush cFrame cStack2 }

rule_Const :: State -> [State]
rule_Const state = maybeToList $ do
  (EvalSlot (Const const), cEnvMem, cStack2) <- stackPopV $ stStack state
  let (mem, heap2) = heapAllocConst const $ stHeap state
  let cFrame = frameMk cEnvMem $ ReturnSlot mem
  return $ state { stStack = stackPush cFrame cStack2
                 , stHeap = heap2 }

rule_Seq :: State -> [State]
rule_Seq state = maybeToList $ do
  (EvalSlot (Seq exprs), cEnvMem, cStack2) <- stackPopV $ stStack state
  let cFrames = map (frameMk cEnvMem . EvalSlot) exprs
  return $ state { stStack = stackPushList cFrames cStack2 }

rule_LamAbs :: State -> [State]
rule_LamAbs state = maybeToList $ do
  (EvalSlot (LamAbs params expr), cEnvMem, cStack2)
    <- stackPopV $ stStack state
  let fEnv = envAssignPred cEnvMem envEmpty
  let fEnvObj = DataObj (EnvVal fEnv) attrsEmpty
  let (fEnvMem, heap2) = heapAlloc fEnvObj $ stHeap state
  let funObj = DataObj (FunVal fEnvMem params expr) attrsEmpty
  let (funMem, heap3) = heapAlloc funObj heap2
  let cFrame = frameMk cEnvMem $ ReturnSlot funMem
  return $ state { stHeap = heap3
                 , stStack = stackPush cFrame cStack2 }

rule_LamAppFun :: State -> [State]
rule_LamAppFun state = maybeToList $ do
  (EvalSlot (LamApp fun args), cEnvMem, cStack2)
    <- stackPopV $ stStack state
  let fFrame = frameMk cEnvMem $ EvalSlot fun
  let cFrame = frameMk cEnvMem $ LamASlot Nothing [] Nothing args
  return $ state { stStack = stackPushList [fFrame, cFrame] cStack2 }

rule_LamAppFunRet :: State -> [State]
rule_LamAppFunRet state = maybeToList $ do
  (ReturnSlot funMem, _,
   LamASlot Nothing [] Nothing args, cEnvMem,
   cStack2) <- stackPopV2 $ stStack state
  let lamASlot = LamASlot (Just funMem) [] Nothing args
  let cFrame = frameMk cEnvMem lamASlot
  return $ state { stStack = stackPush cFrame cStack2 }

rule_LamAppArg :: State -> [State]
rule_LamAppArg state = maybeToList $ do
  (LamASlot (Just funMem) dones Nothing (arg : args),
   cEnvMem, cStack2) <- stackPopV $ stStack state
  let aFrame = frameMk cEnvMem $ EvalSlot $ exprFromArg arg
  let lamASlot = LamASlot (Just funMem) dones (Just arg) args
  let cFrame = frameMk cEnvMem lamASlot
  return $ state { stStack = stackPushList [aFrame, cFrame] cStack2}

rule_LamAppArgRet :: State -> [State]
rule_LamAppArgRet state = maybeToList $ do
  (ReturnSlot argMem, _,
   LamASlot (Just funMem) dones (Just arg) args, cEnvMem,
   cStack2) <- stackPopV2 $ stStack state
  let dones2 = dones ++ [(arg, argMem)]
  let lamASlot = LamASlot (Just funMem) dones2 Nothing args
  let cFrame = frameMk cEnvMem lamASlot
  return $ state { stStack = stackPush cFrame cStack2 }

rule_LamAppEnter :: State -> [State]
rule_LamAppEnter state = maybeToList $ do
  (LamASlot (Just funMem) dones Nothing [], cEnvMem, cStack2)
    <- stackPopV $ stStack state
  -- Get out the things in the heap
  DataObj (FunVal fEnvMem params body) _ <- heapLookup funMem $ stHeap state
  DataObj (EnvVal cEnv) _ <- heapLookup cEnvMem $ stHeap state
  (binds, vars) <- matchLamApp params dones cEnv $ stHeap state
  -- Bind the variadic sand memBinds first
  let (memBinds, exprBinds) = splitBinds binds
  (_, heap2) <- liftVariadics vars fEnvMem $ stHeap state
  heap3 <- heapEnvInsertList memBinds fEnvMem heap2
  -- Now add the expression bindings before the body thing
  let aFrames = map (\(id, expr) ->
                 frameMk fEnvMem $ EvalSlot $ Assign (Var id) expr) exprBinds
  let fFrame = frameMk fEnvMem $ EvalSlot body
  let cFrame = frameMk cEnvMem $ LamBSlot funMem
  return $
    state { stStack = stackPushList (aFrames ++ [fFrame, cFrame]) cStack2
          , stHeap = heap3 }

rule_LamAppRet :: State -> [State]
rule_LamAppRet state = maybeToList $ do
  (ReturnSlot mem, _,
   LamBSlot _, cEnvMem,
   cStack2) <- stackPopV2 $ stStack state
  let cFrame = frameMk cEnvMem $ ReturnSlot mem
  return $ state { stStack = stackPush cFrame cStack2 }

rule_NativeLamApp :: State -> [State]
rule_NativeLamApp state =
  case stackPopV $ stStack state of
    Just (EvalSlot (NativeLamApp _ _), _, _) -> nativeCall state
    _ -> []

rule_AssignId :: State -> [State]
rule_AssignId state = maybeToList $ do
  (EvalSlot (Assign (Var id) expr), cEnvMem, cStack2)
    <- stackPopV $ stStack state
  let eFrame = frameMk cEnvMem $ EvalSlot expr
  let cFrame = frameMk cEnvMem $ AssignSlot id
  return $ state { stStack = stackPushList [eFrame, cFrame] cStack2 }

rule_AssignStr :: State -> [State]
rule_AssignStr state = maybeToList $ do
  (EvalSlot (Assign (Const (StringConst str)) e), cEnvMem, cStack2)
    <- stackPopV $ stStack state
  let cFrame = frameMk cEnvMem $ EvalSlot $ Assign (Var $ idFromString str) e
  return $ state { stStack = stackPush cFrame cStack2 }

rule_AssignRet :: State -> [State]
rule_AssignRet state = maybeToList $ do
  (ReturnSlot mem, _, AssignSlot id, cEnvMem, cStack2)
    <- stackPopV2 $ stStack state
  heap2 <- heapEnvInsert id mem cEnvMem $ stHeap state
  let cFrame = frameMk cEnvMem $ ReturnSlot mem
  return $ state { stHeap = heap2
                 , stStack = stackPush cFrame cStack2 }

rule_If :: State -> [State]
rule_If state = maybeToList $ do
  (EvalSlot (If cond true false), cEnvMem, cStack2)
    <- stackPopV $ stStack state
  let aFrame = frameMk cEnvMem $ EvalSlot cond
  let cFrame = frameMk cEnvMem $ BranchSlot true false
  return $ state { stStack = stackPushList [aFrame, cFrame] cStack2 }

rule_IfRet :: State -> [State]
rule_IfRet state = maybeToList $ do
  (ReturnSlot condMem, _,
   BranchSlot true false, cEnvMem,
   cStack2) <- stackPopV2 $ stStack state
  let branch = if isMemConcTrue condMem $ stHeap state then true else false
  let cFrame = frameMk cEnvMem $ EvalSlot branch
  return $ state { stStack = stackPush cFrame cStack2 }

rule_IfRetSym :: State -> [State]
rule_IfRetSym state = undefined

rule_While :: State -> [State]
rule_While state = maybeToList $ do
  (EvalSlot (While cond body), cEnvMem, cStack2) <- stackPopV $ stStack state
  let dFrame = frameMk cEnvMem $ EvalSlot cond
  let cFrame = frameMk cEnvMem $ LoopSlot cond body LoopStateCond
  return $ state { stStack = stackPushList [dFrame, cFrame] cStack2 }

rule_WhileTrue :: State -> [State]
rule_WhileTrue state = maybeToList $ do
  (ReturnSlot condMem, _,
   LoopSlot cond body LoopStateCond, cEnvMem,
   cStack2) <- stackPopV2 $ stStack state
  _ <- if isMemConcTrue condMem $ stHeap state then Just True else Nothing
  let bFrame = frameMk cEnvMem $ EvalSlot body
  let cFrame = frameMk cEnvMem $ LoopSlot cond body LoopStateBody
  return $ state { stStack = stackPushList [bFrame, cFrame] cStack2 }

rule_WhileBodyRet :: State -> [State]
rule_WhileBodyRet state = maybeToList $ do
  (ReturnSlot _, _,
   LoopSlot cond body LoopStateBody, cEnvMem,
   cStack2) <- stackPopV2 $ stStack state
  let dFrame = frameMk cEnvMem $ EvalSlot cond
  let cFrame = frameMk cEnvMem $ LoopSlot cond body LoopStateCond
  return $ state { stStack = stackPushList [dFrame, cFrame] cStack2 }

rule_WhileFalse :: State -> [State]
rule_WhileFalse state = maybeToList $ do
  (ReturnSlot condMem, _,
   LoopSlot cond body LoopStateCond, cEnvMem,
   cStack2) <- stackPopV2 $ stStack state
  let cFrame = frameMk cEnvMem $ ReturnSlot memNull
  return $ state { stStack = stackPush cFrame cStack2 }

rule_WhileSym :: State -> [State]
rule_WhileSym state = undefined

rule_Break :: State -> [State]
rule_Break state = maybeToList $ do
  (EvalSlot Break, _, cStack2) <- stackPopV $ stStack state
  ((cond, body, _), lEnvMem, lStack) <- unwindToLoopSlot cStack2
  let cFrame = frameMk lEnvMem $ ReturnSlot memNull
  return $ state { stStack = stackPush cFrame cStack2 }

rule_Next :: State -> [State]
rule_Next state = maybeToList $ do
  (EvalSlot Next, _, cStack2) <- stackPopV $ stStack state
  ((cond, body, _), lEnvMem, lStack) <- unwindToLoopSlot cStack2
  let bFrame = frameMk lEnvMem $ EvalSlot body
  let cFrame = frameMk lEnvMem $ LoopSlot cond body LoopStateBody
  return $ state { stStack = stackPushList [bFrame, cFrame] cStack2 }

rule_Return :: State -> [State]
rule_Return state = maybeToList $ do
  (EvalSlot (Return expr), cEnvMem, cStack2) <- stackPopV $ stStack state
  (funMem, lEnvMem, fStack) <- unwindToLamBSlot cStack2
  let rFrame = frameMk cEnvMem $ EvalSlot expr
  let cFrame = frameMk lEnvMem $ LamBSlot funMem
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
  | RuleLamAbs
  | RuleLamApp
  | RuleLamAppFun
  | RuleLamAppFunRet
  | RuleLamAppArg
  | RuleLamAppArgRet
  | RuleLamAppEnter
  | RuleLamAppRet
  | RuleNativeLamApp
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
  , (RuleLamAbs, rule_LamAbs)
  , (RuleLamAppFun, rule_LamAppFun)
  , (RuleLamAppFunRet, rule_LamAppFunRet)
  , (RuleLamAppArg, rule_LamAppArg)
  , (RuleLamAppArgRet, rule_LamAppArgRet)
  , (RuleLamAppEnter, rule_LamAppEnter)
  , (RuleLamAppRet, rule_LamAppRet)
  , (RuleNativeLamApp, rule_NativeLamApp)
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

