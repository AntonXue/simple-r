module SimpleR.Interpreter.Commons.Printers
  ( injStr
  , injBreak
  , injNewline
  , injStrRep
  , ppState
  ) where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

import SimpleR.Language
import SimpleR.Interpreter.Commons.Support
import SimpleR.Interpreter.Commons.SupportUtils

injStr :: String -> [String] -> String
injStr str strs = intercalate str strs

injBreak :: String -> [String] -> String
injBreak str strs = injStr ("\n" ++ str ++ "\n") strs

injStrRep :: String -> Int -> [String] -> String
injStrRep str n strs = injStr (intercalate "" $ take n $ repeat str) strs

injNewline :: [String] -> String
injNewline strs = injStr "\n" strs

injBreakRep :: String -> Int -> [String] -> String
injBreakRep str n strs =
  injStr ("\n" ++ (intercalate "" $ take n $ repeat str) ++ "\n") strs

injIntoList :: [String] -> String
injIntoList strs = "[" ++ (injStr "," strs) ++ "]"

ppMap :: (Show k, Show a) => M.Map k a -> String
ppMap mp = injNewline $ map show $ M.toList mp

ppSet :: (Show a) => S.Set a -> String
ppSet set = show $ S.toList set

ppList :: (Show a) => [a] -> String
ppList lst = injNewline $ map show lst

---

ppMem :: MemAddr -> String
ppMem mem = "Mem (" ++ (show $ memAddr mem) ++ ")"

ppId :: Ident -> String
ppId id = case idPkg id of
  Just pkg -> "Id " ++ (show $ pkg ++ "::" ++ idName id) ++ ")"
  _ -> "Id (" ++ (show $ idName id) ++ ")"

ppExpr :: Expr -> String
ppExpr expr = show expr

ppRedex :: Redex -> String
ppRedex (BlankRed) = "BlankRed"
ppRedex (ResultRed mem) = "ResultRed (" ++ ppMem mem ++ ")"
ppRedex (EvalRed envMem expr) =
  "EvalRed @ " ++ ppMem envMem ++ " (" ++ ppExpr expr ++ ")"

ppEnv :: Env -> String
ppEnv env =
  let headerStr = ">> Env (Pred: " ++ (ppMem $ envPredMem env) in
  let mapStr = ppMap $ envMap env in
    injNewline [headerStr, mapStr]

ppCont :: Cont -> String
ppCont slot = show slot

ppFrame :: Frame -> String
ppFrame frame =
  let headerStr = ">>>>> Frame " ++ (ppMem $ frameEnvMem frame) in
  let slotStr = ppCont $ frameCont frame in
    injNewline [headerStr, slotStr]

ppStack :: Stack -> String
ppStack stack =
  let headerStr = ">> Stack (Reverse dump)" in
  -- let frames = map ppFrame $ stackList stack in
  let framesStr = reverse $ map ppFrame $ stackList stack in
    injBreakRep "-" 20 $ headerStr : framesStr

ppAttrs :: Attributes -> String
ppAttrs attrs = show attrs

ppHeapObj :: HeapObj -> String
ppHeapObj (PromiseObj mem thunk) =
  let headerStr = "PromiseObj " ++ (ppMem mem) in
  let exprStr = ppExpr thunk in
    injNewline [headerStr, exprStr]
ppHeapObj (DataObj (VecVal vec) attrs) =
  let headerStr = "VectorVal" in
  let vecStr = show vec in
  let attrStr = ppAttrs attrs in
    injNewline [headerStr, vecStr, attrStr]
ppHeapObj (DataObj (RefsVal mems) attrs) =
  let headerStr = "RefsVal" in
  let mapStr = injIntoList $ map ppMem mems in
  let attrStr = ppAttrs attrs in
    injNewline $ [headerStr, mapStr, attrStr]
ppHeapObj (DataObj (FunVal mem params expr) attrs) =
  let headerStr = "FunVal" in
  let memStr = ppMem mem in
  let paramStr = show params in
  let exprStr = ppExpr expr in
  let attrStr = ppAttrs attrs in
    injNewline $ [headerStr, memStr, paramStr, exprStr, attrStr]
ppHeapObj (DataObj (EnvVal env) attrs) =
  let headerStr = "EnvVal" in
  let envStr = ppEnv env in
  let attrStr = ppAttrs attrs in
    injNewline $ [headerStr, envStr, attrStr]

ppHeap :: Heap -> String
ppHeap heap =
  let headerStr = ">> Heap (next: " ++ (ppMem $ heapNextMem heap) in
  let entriesStr = map (\(mem, hobj) ->
                  let memHeader = "Heap @ [" ++ ppMem mem ++ "]" in
                    injNewline [memHeader, ppHeapObj hobj]) $
                M.toList $ heapMap heap in
    injBreakRep "-" 20 $ headerStr : entriesStr

ppPures :: Pures -> String
ppPures pures =
  let headerStr = ">> Pures" in
  let idsStr = injIntoList $ map ppId $ S.toList $ puresSet pures in
    injNewline $ [headerStr, idsStr]

ppState :: State -> String
ppState state =
  let headerStr = "> State" in
  let gMemStr = "G: (" ++ (ppMem $ stGlobalEnvMem state) ++ ")" in
  let pMemStr = "P: (" ++ (ppMem $ stBaseEnvMem state) ++ ")" in
  let heapStr = ppHeap $ stHeap state in
  let stackStr = ppStack $ stStack state in
  let redexStr = ppRedex $ stRedex state in
    injBreakRep "*" 50 $
      [headerStr, gMemStr, pMemStr, heapStr, stackStr, redexStr]


