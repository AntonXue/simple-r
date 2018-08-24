{-# LANGUAGE ScopedTypeVariables #-}

module SimpleR.Interpreter.Stepper.Strict
  ( Rule(..)
  , rulePairs
  ) where

import Data.Maybe

import SimpleR.Language
import SimpleR.Interpreter.Commons
import SimpleR.Interpreter.Stepper.ParamArgMatching
import SimpleR.Interpreter.Natives
import SimpleR.Smt

-- Helper functions
splitEithers :: [Either a b] -> ([a], [b])
splitEithers [] = ([], [])
splitEithers (e : es) =
  let (as, bs) = splitEithers es in
    case e of
      Left a -> (a : as, bs)
      Right b -> (as, b : bs)

splitBinds ::
  [(Ident, Either MemAddr Expr)] -> ([(Ident, MemAddr)], [(Ident, Expr)])
splitBinds binds =
  foldl
    (\(accL, accR) (id, rhs) ->
      case rhs of
        Left mem -> (accL ++ [(id, mem)], accR)
        Right expr -> (accL, accR ++ [(id, expr)]))
    ([], []) binds

isMemConcTrue :: MemAddr -> Heap -> Maybe Bool
isMemConcTrue mem heap =
  case heapLookup mem heap of
    Just (DataObj (VecVal vec) _) ->
      case vec of
        (IntVec (((Atom x) : _) :: [Atom Int])) -> Just $ x /= 0
        (DoubleVec (((Atom x) : _) :: [Atom Double])) -> Just $ x /= 0
        (ComplexVec (((Atom x) : _) :: [Atom Complex])) -> Just $ x /= 0
        (BoolVec ((Atom x) : _)) -> Just x
        _ -> Nothing

symIdFromMem :: MemAddr -> Heap -> Maybe SmtIdent
symIdFromMem mem heap =
  case heapLookup mem heap of
    Just (DataObj (VecVal (SymVec sid _ _)) _) -> Just sid
    _ -> Nothing

exprFromArg :: Arg -> Expr
exprFromArg (Arg expr) = expr
exprFromArg (Named _ expr) = expr
exprFromArg (VarArg) = Var idVariadic

padBlankNames :: [Either MemAddr (Ident, MemAddr)] -> [(String, MemAddr)]
padBlankNames [] = []
padBlankNames ((Left mem) : args) = ("", mem) : padBlankNames args
padBlankNames ((Right (id, mem)) : args) = (idName id, mem) : padBlankNames args

liftVariadics ::
  [Either MemAddr (Ident, MemAddr)] -> MemAddr -> Heap -> Maybe (MemAddr, Heap)
liftVariadics args envMem heap = do
  let (names, mems) = unzip $ padBlankNames args
  let namesVal = VecVal $ StringVec (map Atom names)
  let (namesMem, heap2) = heapAlloc (DataObj namesVal attrsEmpty) heap
  let varVal = RefsVal mems
  let refsAttrs = attrsInsert idAttrsNames namesMem attrsEmpty
  let (refsMem, heap3) = heapAlloc (DataObj varVal refsAttrs) heap2
  heap4 <- heapEnvInsert idVariadic refsMem envMem heap3
  return (refsMem, heap4)

unwindToLoopCont :: Stack -> Maybe ((Expr, Expr, LoopConfig), MemAddr, Stack)
unwindToLoopCont stack = do
  (cont, lEnvMem, stack2) <- stackPopV stack
  case cont of
    LoopCont cond body loop -> return ((cond, body, loop), lEnvMem, stack2)
    _ -> unwindToLoopCont stack2

unwindToLamBCont :: Stack -> Maybe (MemAddr, MemAddr, Stack)
unwindToLamBCont stack = do
  (cont, lEnvMem, stack2) <- stackPopV stack
  case cont of
    LamBCont funMem -> return (funMem, lEnvMem, stack2)
    _ -> unwindToLamBCont stack2

----------------------------
-- Rules
rule_Ident :: State -> [State]
rule_Ident state
  | EvalRed envMem (Var id) <- stRedex state =
      let lookupFun = case stackPopV $ stStack state of
            Just (LamACont _ _ _ _, _, _) -> heapEnvLookupDeepFun
            _ -> heapEnvLookupDeep in
      let mem = fromMaybe memNull $ lookupFun envMem id $ stHeap state in
        [state { stRedex = ResultRed mem }]
  | otherwise = []

rule_Const :: State -> [State]
rule_Const state
  | EvalRed envMem (Const const) <- stRedex state =
      let (mem, heap2) = heapAllocConst const $ stHeap state in
        [state { stRedex = ResultRed mem
               , stHeap = heap2 }]
  | otherwise = []

rule_SeqEmpty :: State -> [State]
rule_SeqEmpty state
  | EvalRed envMem (Seq []) <- stRedex state =
      [state { stRedex = ResultRed memNull }]
  | otherwise = []

rule_Seq :: State -> [State]
rule_Seq state
  | EvalRed envMem (Seq (expr : exprs)) <- stRedex state =
      let frames = map (frameMk envMem . ExprCont) exprs in
      let stack2 = stackPushList frames $ stStack state in
        [state { stRedex = EvalRed envMem expr
               , stStack = stack2 }]
  | otherwise = []

rule_LamAbs :: State -> [State]
rule_LamAbs state
  | EvalRed envMem (LamAbs params expr) <- stRedex state =
      let funObj = DataObj (FunVal envMem params expr) attrsEmpty in
      let (funMem, heap2) = heapAlloc funObj $ stHeap state in
        [state { stRedex = ResultRed funMem
               , stHeap = heap2 }]
  | otherwise = []

rule_LamAppFun :: State -> [State]
rule_LamAppFun state
  | EvalRed envMem (LamApp fun args) <- stRedex state =
      let frame = frameMk envMem $ LamACont Nothing [] Nothing args in
        [state { stRedex = EvalRed envMem fun
               , stStack = stackPush frame $ stStack state }]
  | otherwise = []

rule_LamAppFunRet :: State -> [State]
rule_LamAppFunRet state
  | ResultRed funMem <- stRedex state
  , Just (LamACont Nothing [] Nothing args, envMem, stack2)
      <- stackPopV $ stStack state =
      let lamACont = LamACont (Just funMem) [] Nothing args in
      let frame = frameMk envMem lamACont in
        [state { stRedex = BlankRed
               , stStack = stackPush frame stack2 }]
  | otherwise = []

rule_LamAppArg :: State -> [State]
rule_LamAppArg state
  | BlankRed <- stRedex state
  , Just (LamACont (Just funMem) dones Nothing (arg : args), envMem, stack2)
      <- stackPopV $ stStack state =
      let argExpr = exprFromArg arg in
      let lamACont = LamACont (Just funMem) dones (Just arg) args in
      let frame = frameMk envMem lamACont in
        [state { stRedex = EvalRed envMem argExpr
               , stStack = stackPush frame stack2 }]
  | otherwise = []

rule_LamAppArgRet :: State -> [State]
rule_LamAppArgRet state
  | ResultRed argMem <- stRedex state
  , Just (LamACont (Just funMem) dones (Just arg) args, envMem, stack2)
      <- stackPopV $ stStack state =
      let dones2 = dones ++ [(arg, argMem)] in
      let lamACont = LamACont (Just funMem) dones2 Nothing args in
      let frame = frameMk envMem lamACont in
        [state { stRedex = BlankRed
               , stStack = stackPush frame stack2 }]

  | otherwise = []

rule_LamAppEnter :: State -> [State]
rule_LamAppEnter state
  | BlankRed <- stRedex state
  , Just (LamACont (Just funMem) dones Nothing [], envMem, stack2)
      <- stackPopV $ stStack state
  , Just (DataObj (FunVal funPredEnvMem params body) _)
      <- heapLookup funMem $ stHeap state
  , Just (DataObj (EnvVal env) _) <- heapLookup envMem $ stHeap state =
    maybeToList $ do
      -- Make a new environment for the function
      let funEnv = envAssignPred funPredEnvMem envEmpty
      let funEnvObj = DataObj (EnvVal funEnv) attrsEmpty
      let (funEnvMem, heap2) = heapAlloc funEnvObj $ stHeap state
      -- Do some matching
      -- matchLamApp should happen in the current environment
      (binds, variadics) <- matchLamApp params dones env heap2
      let (memBinds, exprBinds) = splitBinds binds
      (_, heap3) <- liftVariadics variadics funEnvMem heap2
      heap4 <- heapEnvInsertList memBinds funEnvMem heap3
      -- Make the continuations for assignments
      let aConts = map (\(id, ex) -> ExprCont $ Assign (Var id) ex) exprBinds
      let aFrames = map (frameMk funEnvMem) aConts
      -- Body frame
      let bFrame = frameMk funEnvMem $ ExprCont body
      let lamBFrame = frameMk envMem $ LamBCont funMem
      let stack3 = stackPushList (aFrames ++ [bFrame, lamBFrame]) stack2
      return $ state { stRedex = ResultRed memNull -- In case nothing happens
                     , stStack = stack3
                     , stHeap = heap4 }
  | otherwise = []

rule_LamAppRet :: State -> [State]
rule_LamAppRet state
  | ResultRed mem <- stRedex state
  , Just (LamBCont _, _, stack2) <- stackPopV $ stStack state =
      [state { stRedex = ResultRed mem
             , stStack = stack2 }]
  | otherwise = []

rule_NativeLamApp :: State -> [State]
rule_NativeLamApp state
  | EvalRed envMem (NativeLamApp _ _) <- stRedex state = nativeCall state
  | otherwise = []

rule_AssignId :: State -> [State]
rule_AssignId state
  | EvalRed envMem (Assign (Var id) expr) <- stRedex state =
      let frame = frameMk envMem $ AssignCont id in
        [state { stRedex = EvalRed envMem expr
               , stStack = stackPush frame $ stStack state }]
  | otherwise = []

rule_AssignRet :: State -> [State]
rule_AssignRet state
  | ResultRed mem <- stRedex state
  , Just (AssignCont id, envMem, stack2) <- stackPopV $ stStack state =
      maybeToList $ do
      (mem2, heap2) <- heapCopy mem $ stHeap state
      heap3 <- heapEnvInsert id mem2 envMem heap2
      return $ state { stRedex = ResultRed mem2
                     , stStack = stack2
                     , stHeap = heap3 }
  | otherwise = []

rule_If :: State -> [State]
rule_If state
  | EvalRed envMem (If exprC exprT exprF) <- stRedex state =
      let frame = frameMk envMem $ BranchCont exprT exprF in
        [state { stRedex = EvalRed envMem exprC
               , stStack = stackPush frame $ stStack state }]
  | otherwise = []

rule_IfRet :: State -> [State]
rule_IfRet state
  | ResultRed mem <- stRedex state
  , Just (BranchCont exprT exprF, envMem, stack2)
      <- stackPopV $ stStack state
  , Just memVal <- isMemConcTrue mem $ stHeap state =
      let expr = if memVal then exprT else exprF in
        [state { stRedex = EvalRed envMem expr
               , stStack = stack2 }]
  | otherwise = []

rule_IfRetSym :: State -> [State]
rule_IfRetSym state
  | ResultRed mem <- stRedex state
  , Just (BranchCont exprT exprF, envMem, stack2)
      <- stackPopV $ stStack state
  , Just sym <- symIdFromMem mem $ stHeap state =
      error "IMPLEMENT THIS THING"
  | otherwise = []

rule_While :: State -> [State]
rule_While state
  | EvalRed envMem (While cond body) <- stRedex state =
      let frame = frameMk envMem $ LoopCont cond body LoopConfigCond in
        [state { stRedex = EvalRed envMem cond
               , stStack = stackPush frame $ stStack state }]
  | otherwise = []

rule_WhileTrue :: State -> [State]
rule_WhileTrue state
  | ResultRed mem <- stRedex state
  , Just (LoopCont cond body LoopConfigCond, envMem, stack2)
      <- stackPopV $ stStack state
  , Just True <- isMemConcTrue mem $ stHeap state =
      let frame = frameMk envMem $ LoopCont cond body LoopConfigBody in
        [state { stRedex = EvalRed envMem body
               , stStack = stackPush frame stack2 }]
  | otherwise = []

rule_WhileBodyRet :: State -> [State]
rule_WhileBodyRet state
  | ResultRed mem <- stRedex state
  , Just (LoopCont cond body LoopConfigBody, envMem, stack2)
      <- stackPopV $ stStack state =
      let frame = frameMk envMem $ LoopCont cond body LoopConfigCond in
        [state { stRedex = EvalRed envMem cond
               , stStack = stackPush frame stack2 }]
  | otherwise = []

rule_WhileFalse :: State -> [State]
rule_WhileFalse state
  | ResultRed mem <- stRedex state
  , Just (LoopCont cont body LoopConfigCond, envMem, stack2)
      <- stackPopV $ stStack state
  , Just False <- isMemConcTrue mem $ stHeap state =
      [state { stRedex = ResultRed memNull
             , stStack = stack2 }]
  | otherwise = []

rule_WhileSym :: State -> [State]
rule_WhileSym state
  | ResultRed mem <- stRedex state
  , Just (LoopCont cont body LoopConfigCond, envMem, stack2)
      <- stackPopV $ stStack state
  , Just sym <- symIdFromMem mem $ stHeap state =
      error "WHILE FALSE"
  | otherwise = []

rule_BreakStop :: State -> [State]
rule_BreakStop state
  | EvalRed envMem Break <- stRedex state
  , Just (LoopCont _ _ _, _, stack2) <- stackPopV $ stStack state =
      [state { stRedex = ResultRed memNull
             , stStack = stack2 }]
  | otherwise = []

rule_BreakGo :: State -> [State]
rule_BreakGo state
  | EvalRed envMem Break <- stRedex state
  , Just (cont, _, stack2) <- stackPopV $ stStack state =
      let go = case cont of
                  ExprCont _ -> True
                  BranchCont _ _ -> True
                  _ -> False in
        [state { stRedex = EvalRed envMem Break
               , stStack = stack2 }]
  | otherwise = []

rule_NextStop :: State -> [State]
rule_NextStop state
  | EvalRed envMem Next <- stRedex state
  , Just (LoopCont cond body _, envMem, stack2) <- stackPopV $ stStack state =
      let frame = frameMk envMem $ LoopCont cond body LoopConfigCond in
        [state { stRedex = EvalRed envMem cond
               , stStack = stackPush frame stack2 }]
  | otherwise = []

rule_NextGo :: State -> [State]
rule_NextGo state
  | EvalRed envMem Next <- stRedex state
  , Just (cont, _, stack2) <- stackPopV $ stStack state =
      let go = case cont of
                  ExprCont _ -> True
                  BranchCont _ _ -> True
                  _ -> False in
        [state { stRedex = EvalRed envMem Next
               , stStack = stack2 }]
  | otherwise = []

rule_ReturnStop :: State -> [State]
rule_ReturnStop state
  | EvalRed envMem (Return expr) <- stRedex state
  , Just (LamBCont funMem, lamEnvMem, stack2) <- stackPopV $ stStack state =
      let frame = frameMk lamEnvMem $ LamBCont funMem in
        [state { stRedex = EvalRed envMem expr
               , stStack = stackPush frame stack2 }]
  | otherwise = []

rule_ReturnGo :: State -> [State]
rule_ReturnGo state
  | EvalRed envMem (Return expr) <- stRedex state
  , Just (cont, _, stack2) <- stackPopV $ stStack state =
      let go = case cont of
                  ExprCont _ -> True
                  BranchCont _ _ -> True
                  LoopCont _ _ _ -> True
                  _ -> False in
        [state { stRedex = EvalRed envMem (Return expr)
               , stStack = stack2 }]
  | otherwise = []

rule_ResultDiscard :: State -> [State]
rule_ResultDiscard state
  | (ResultRed _) <- stRedex state
  , Just (ExprCont expr, envMem, stack2) <- stackPopV $ stStack state =
      [state { stRedex = EvalRed envMem expr
             , stStack = stack2 }]
  | otherwise = []

-- Need to change the 4 x 2 = 8 access get and setters to function calls
rule_VecInd :: State -> [State]
rule_VecInd state
  | (EvalRed envMem (VecInd expr args)) <- stRedex state =
      let fun = Var $ idFromRPrim RPrimVecInd in
      let redExpr = LamApp fun $ (Arg expr) : args in
        [state { stRedex = EvalRed envMem redExpr }]
  | otherwise = []

rule_VecIndAssign :: State -> [State]
rule_VecIndAssign state
  | (EvalRed envMem (VecIndAssign expr1 args expr2)) <- stRedex state =
      let fun = Var $ idFromRPrim RPrimVecIndAssign in
      let redExpr = LamApp fun $ (map Arg [expr1, expr2]) ++ args in
        [state { stRedex = EvalRed envMem redExpr }]
  | otherwise = []

rule_VecSub :: State -> [State]
rule_VecSub state
  | (EvalRed envMem (VecSub expr args)) <- stRedex state =
      let fun = Var $ idFromRPrim RPrimVecSub in
      let redExpr = LamApp fun $ (Arg expr) : args in
        [state { stRedex = EvalRed envMem redExpr }]
  | otherwise = []

rule_VecSubAssign :: State -> [State]
rule_VecSubAssign state
  | (EvalRed envMem (VecSubAssign expr1 args expr2)) <- stRedex state =
      let fun = Var $ idFromRPrim RPrimVecSubAssign in
      let redExpr = LamApp fun $ (map Arg [expr1, expr2]) ++ args in
        [state { stRedex = EvalRed envMem redExpr }]
  | otherwise = []

rule_ListName :: State -> [State]
rule_ListName state
  | (EvalRed envMem (ListName expr1 expr2)) <- stRedex state =
      let fun = Var $ idFromRPrim RPrimListName in
      let redExpr = LamApp fun $ (map Arg [expr1, expr2]) in
        [state { stRedex = EvalRed envMem redExpr }]
  | otherwise = []

rule_ListNameAssign :: State -> [State]
rule_ListNameAssign state
  | (EvalRed envMem (ListNameAssign expr1 expr2 expr3)) <- stRedex state =
      let fun = Var $ idFromRPrim RPrimListNameAssign in
      let redExpr = LamApp fun $ (map Arg [expr1, expr2, expr3]) in
        [state { stRedex = EvalRed envMem redExpr }]
  | otherwise = []

rule_ObjAttr :: State -> [State]
rule_ObjAttr state
  | (EvalRed envMem (ObjAttr expr1 expr2)) <- stRedex state =
      let fun = Var $ idFromRPrim RPrimObjAttr in
      let redExpr = LamApp fun $ (map Arg [expr1, expr2]) in
        [state { stRedex = EvalRed envMem redExpr }]
  | otherwise = []

rule_ObjAttrAssign :: State -> [State]
rule_ObjAttrAssign state
  | (EvalRed envMem (ObjAttrAssign expr1 expr2 expr3)) <- stRedex state =
      let fun = Var $ idFromRPrim RPrimObjAttrAssign in
      let redExpr = LamApp fun $ (map Arg [expr1, expr2, expr3]) in
        [state { stRedex = EvalRed envMem redExpr }]
  | otherwise = []

rule_Blank :: State -> [State]
rule_Blank _ = []


data Rule =
    RuleIdent
  | RuleConst
  | RuleSeqEmpty
  | RuleSeq
  | RuleLamAbs
  | RuleLamAppFun
  | RuleLamAppFunRet
  | RuleLamAppArg
  | RuleLamAppArgRet
  | RuleLamAppEnter
  | RuleLamAppRet
  | RuleNativeLamApp
  | RuleAssignId
  | RuleAssignRet
  | RuleIf
  | RuleIfRet
  | RuleIfRetSym
  | RuleWhile
  | RuleWhileTrue
  | RuleWhileBodyRet
  | RuleWhileFalse
  | RuleWhileSym
  | RuleBreakStop
  | RuleBreakGo
  | RuleNextStop
  | RuleNextGo
  | RuleReturnStop
  | RuleReturnGo
  | RuleResultDiscard
  
  | RuleVecInd
  | RuleVecIndAssign
  | RuleVecSub
  | RuleVecSubAssign
  | RuleListName
  | RuleListNameAssign
  | RuleObjAttr
  | RuleObjAttrAssign

  | RuleBlank
  deriving (Ord, Eq, Show, Read)

rulePairs :: [(Rule, State -> [State])]
rulePairs =
  [ 
    (RuleIdent, rule_Ident)
  , (RuleConst, rule_Const)
  , (RuleSeqEmpty, rule_SeqEmpty)
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
  , (RuleAssignRet, rule_AssignRet)
  , (RuleIf, rule_If)
  , (RuleIfRet, rule_IfRet)
  , (RuleIfRetSym, rule_IfRetSym)
  , (RuleWhile, rule_While)
  , (RuleWhileTrue, rule_WhileTrue)
  , (RuleWhileBodyRet, rule_WhileBodyRet)
  , (RuleWhileFalse, rule_WhileFalse)
  , (RuleWhileSym, rule_WhileSym)
  , (RuleBreakStop, rule_BreakStop)
  , (RuleBreakGo, rule_BreakGo)
  , (RuleNextStop, rule_NextStop)
  , (RuleNextGo, rule_NextGo)
  , (RuleReturnStop, rule_ReturnStop)
  , (RuleReturnGo, rule_ReturnGo)
  , (RuleResultDiscard, rule_ResultDiscard)

  , (RuleVecInd, rule_VecInd)
  , (RuleVecIndAssign, rule_VecIndAssign)
  , (RuleVecSub, rule_VecSub)
  , (RuleVecSubAssign, rule_VecSubAssign)
  , (RuleListName, rule_ListName)
  , (RuleListNameAssign, rule_ListNameAssign)
  , (RuleObjAttr, rule_ObjAttr)
  , (RuleObjAttrAssign, rule_ObjAttrAssign)

  , (RuleBlank, rule_Blank) ]
 
