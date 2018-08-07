module SimpleR.Interpreter.Commons.Printers
  ( ppState
  ) where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

import SimpleR.Language
import SimpleR.Interpreter.Commons.Support
import SimpleR.Interpreter.Commons.SupportUtils

injStr :: String -> [String] -> String
injStr str strs = intercalate str strs

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

ppMem :: MemRef -> String
ppMem mem = "Mem (" ++ (show $ memAddr mem) ++ ")"

ppId :: Ident -> String
ppId id = case idPkg id of
  Just pkg -> "Id " ++ (show $ pkg ++ "::" ++ idName id) ++ ")"
  _ -> "Id (" ++ (show $ idName id) ++ ")"

ppExpr :: Expr -> String
ppExpr expr = show expr

ppEnv :: Env -> String
ppEnv env =
  let header = ">> Env (Pred: " ++ (ppMem $ envPredMem env) in
  let map = ppMap $ envMap env in
    injNewline [header, map]

ppSlot :: Slot -> String
ppSlot slot = show slot

ppFrame :: Frame -> String
ppFrame frame =
  let header = ">>>>> Frame " ++ (ppMem $ frameEnvMem frame) in
  let slot = ppSlot $ frameSlot frame in
    injNewline [header, slot]

ppStack :: Stack -> String
ppStack stack =
  let header = ">> Stack" in
  let frames = map ppFrame $ stackList stack in
    injBreakRep "-" 20 $ header : frames

ppAttrs :: Attributes -> String
ppAttrs attrs = show attrs

ppHeapObj :: HeapObj -> String
ppHeapObj (PromiseObj mem thunk) =
  let header = "PromiseObj " ++ (ppMem mem) in
  let expr = ppExpr thunk in
    injNewline [header, expr]
ppHeapObj (DataObj (VecVal vec) attrs) =
  let header = "VectorVal" in
  let vec = show vec in
  let att = ppAttrs attrs in
    injNewline [header, vec, att]
ppHeapObj (DataObj (RefsVal mems) attrs) =
  let header = "RefsVal" in
  let ms = injIntoList $ map ppMem mems in
  let att = ppAttrs attrs in
    injNewline $ [header, ms, att]
ppHeapObj (DataObj (FunVal mem params expr) attrs) =
  let header = "FunVal" in
  let m = ppMem mem in
  let paramStr = show params in
  let exprStr = ppExpr expr in
  let att = ppAttrs attrs in
    injNewline $ [header, m, paramStr, exprStr, att]
ppHeapObj (DataObj (EnvVal env) attrs) =
  let header = "EnvVal" in
  let e = ppEnv env in
  let att = ppAttrs attrs in
    injNewline $ [header, e, att]

ppHeap :: Heap -> String
ppHeap heap =
  let header = ">> Heap (next: " ++ (ppMem $ heapNextMem heap) in
  let entries = map (\(mem, hobj) ->
                  let memHeader = "Heap @ [" ++ ppMem mem ++ "]" in
                    injNewline [memHeader, ppHeapObj hobj]) $
                M.toList $ heapMap heap in
    injBreakRep "-" 20 $ header : entries

ppPures :: Pures -> String
ppPures pures =
  let header = ">> Pures" in
  let ids = injIntoList $ map ppId $ S.toList $ puresSet pures in
    injNewline $ [header, ids]

ppState :: State -> String
ppState state =
  let header = "> State (G: " ++ (ppMem $ stGlobalEnvMem state) ++
                     ") (P: " ++ (ppMem $ stBaseEnvMem state) ++ ")" in
  let heap = ppHeap $ stHeap state in
  let stack = ppStack $ stStack state in
    injBreakRep "*" 50 $
      [header, heap, stack]


