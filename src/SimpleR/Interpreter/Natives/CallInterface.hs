module SimpleR.Interpreter.Natives.CallInterface
  ( nativeCall
  ) where

import qualified Data.Map as M

import SimpleR.Language
import SimpleR.Interpreter.Commons
import SimpleR.Interpreter.Natives.RPrimitives
import SimpleR.Interpreter.Natives.PrimNumLogOps
import SimpleR.Interpreter.Natives.PrimVecManipOps

nativeCallError :: RPrim -> String -> a
nativeCallError prim str =
  error $ "nativeCall @ " ++ show prim ++ ": " ++ str

-------
-- Register the native calls here.
-- Calls to the primitive functions should modify _ONLY_ the heap
-- It's up the the `nativeCall` function to decide how to handle
-- the returned values.
nativeCall :: State -> [State]
nativeCall state
  | EvalRed envMem (NativeLamApp fun args) <- stRedex state
  , Just funMem <- heapEnvLookupDeepFun envMem fun $ stHeap state
  , Just argMems
      <- mapM (\a -> heapEnvLookupDeep envMem a (stHeap state)) args =
      case M.lookup fun idPrimMap of
        Just RPrimC ->
          case argMems of
            (refsMem : []) -> prim_C refsMem state
            _ -> nativeCallError RPrimC $ show argMems

        Just RPrimLength ->
          case argMems of
            (vecMem : []) -> prim_Length vecMem state
            _ -> nativeCallError RPrimLength $ show argMems

        Just RPrimColon ->
          case argMems of
            (lowMem : highMem : []) -> prim_Colon lowMem highMem state
            _ -> nativeCallError RPrimColon $ show argMems

        -- Numerical operations
        Just RPrimPlus ->
          case argMems of
            (vecMem1 : []) -> prim_UPlus vecMem1 state
            (vecMem1 : vecMem2 : []) ->
              prim_BinOp vecMem1 vecMem2 plusBinOp state
            _ -> nativeCallError RPrimPlus $ show argMems

        Just RPrimMinus ->
          case argMems of
            (vecMem1 : []) -> prim_UNotOp vecMem1 state
            (vecMem1 : vecMem2 : []) ->
              prim_BinOp vecMem1 vecMem2 subBinOp state
            _ -> nativeCallError RPrimMinus $ show argMems

        Just RPrimMult ->
          case argMems of
            (vecMem1 : vecMem2 : []) ->
              prim_BinOp vecMem1 vecMem2 multBinOp state
            _ -> nativeCallError RPrimMult $ show argMems

        Just RPrimDiv ->
          case argMems of
            (vecMem1 : vecMem2 : []) ->
              prim_BinOp vecMem1 vecMem2 divBinOp state
            _ -> nativeCallError RPrimDiv $ show argMems

        Just RPrimPow ->
          case argMems of
            (vecMem1 : vecMem2 : []) ->
              prim_BinOp vecMem1 vecMem2 powBinOp state
            _ -> nativeCallError RPrimPow $ show argMems

        Just RPrimMod ->
          case argMems of
            (vecMem1 : vecMem2 : []) ->
              prim_BinOp vecMem1 vecMem2 modBinOp state
            _ -> nativeCallError RPrimMod $ show argMems

        -- Logical operations

        Just RPrimAnd ->
          case argMems of
            (vecMem1 : vecMem2 : []) ->
              prim_BinOp vecMem1 vecMem2 andBinOp state
            _ -> nativeCallError RPrimAnd $ show argMems

        Just RPrimOr ->
          case argMems of
            (vecMem1 : vecMem2 : []) ->
              prim_BinOp vecMem1 vecMem2 orBinOp state
            _ -> nativeCallError RPrimOr $ show argMems

        Just RPrimEq ->
          case argMems of
            (vecMem1 : vecMem2 : []) ->
              prim_BinOp vecMem1 vecMem2 eqBinOp state
            _ -> nativeCallError RPrimEq $ show argMems

        Just RPrimNeq ->
          case argMems of
            (vecMem1 : vecMem2 : []) ->
              prim_BinOp vecMem1 vecMem2 neqBinOp state
            _ -> nativeCallError RPrimNeq $ show argMems

        Just RPrimLt ->
          case argMems of
            (vecMem1 : vecMem2 : []) ->
              prim_BinOp vecMem1 vecMem2 ltBinOp state
            _ -> nativeCallError RPrimLt $ show argMems

        Just RPrimLe ->
          case argMems of
            (vecMem1 : vecMem2 : []) ->
              prim_BinOp vecMem1 vecMem2 leBinOp state
            _ -> nativeCallError RPrimLe $ show argMems

        Just RPrimGe ->
          case argMems of
            (vecMem1 : vecMem2 : []) ->
              prim_BinOp vecMem1 vecMem2 geBinOp state
            _ -> nativeCallError RPrimGe $ show argMems

        Just RPrimGt ->
          case argMems of
            (vecMem1 : vecMem2 : []) ->
              prim_BinOp vecMem1 vecMem2 gtBinOp state
            _ -> nativeCallError RPrimGt $ show argMems


  | otherwise = []

