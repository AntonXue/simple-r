module SimpleR.Interpreter.Natives.CallInterface
  ( nativeCall
  ) where

import qualified Data.Map as M

import SimpleR.Language
import SimpleR.Interpreter.Commons
import SimpleR.Interpreter.Natives.RPrimitives
import SimpleR.Interpreter.Natives.PrimVecManipOps


nativeCall :: State -> [State]
nativeCall state
  | EvalRed envMem (NativeLamApp fun args) <- stRedex state
  , Just funMem <- heapEnvLookupDeepFun envMem fun $ stHeap state
  , Just argMems
      <- mapM (\a -> heapEnvLookupDeep envMem a (stHeap state)) args =
      case M.lookup fun idPrimMap of
        Just RPrimC ->
          case argMems of
            (refsMem : []) ->
              case prim_C refsMem state of
                ((m, s) : []) -> [s { stRedex = ResultRed m }]
                _ -> error $ "nativeCall: RPrimC call failed"
            _ -> error $ "nativeCall: RPrimC with " ++ show argMems
        _ -> []

  | otherwise = []

