module SimpleR.Interpreter.Natives.CallInterface
  ( nativeCall
  ) where

import qualified Data.Map as M

import SimpleR.Language
import SimpleR.Interpreter.Commons
import SimpleR.Interpreter.Natives.RPrimitives
import SimpleR.Interpreter.Natives.PrimVecManipOps


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
            _ -> error $ "nativeCall: RPrimC with " ++ show argMems

        Just RPrimLength ->
          case argMems of
            (vecMem : []) -> prim_Length vecMem state
            _ -> error $ "nativeCall: RPrimLength with " ++ show argMems

        Just RPrimColon ->
          case argMems of
            (lowMem : highMem : []) -> prim_Colon lowMem highMem state
            _ -> error $ "nativeCall: RPrimColon with " ++ show argMems
        _ -> []

  | otherwise = []

