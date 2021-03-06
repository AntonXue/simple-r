module SimpleR.Interpreter.Natives.PrimVecManipOps

  where

import Debug.Trace

import SimpleR.Language
import SimpleR.Interpreter.Commons

--   -- Vectors
--   , ("c",           RPrimC,              [])
--   , (":",           RPrimColon,          [])
--   , ("[[",  RPrimVecProj,         [])
--   , ("[",   RPrimVecSub,          [])
--   , ("dim",         RPrimDim,            [])
--   , ("dim<-",       RPrimDimAssign,       [])
--   , ("dimnames",    RPrimDimNames,       [])
--   , ("dimnames<-",  RPrimDimNamesAssign, [])
--   , ("length",      RPrimLength,        [])
--   , ("length<-",    RPrimLengthAssign,  [])
--   , ("levels",      RPrimLevels,        [])
--   , ("levels<-",    RPrimLevelsAssign,  [])
--   , ("names",       RPrimNames,         [])
--   , ("names<-",     RPrimNamesAssign,   [])
--   , ("rep",         RPrimRep,     [])
--   , ("seq.int",     RPrimSeqInt,  [])
--   , ("xtfrm",       RPrimXtfrm,   [])
--   , ("anyNA",           RPrimAnyNa,         [])



--   , ("c",           RPrimC,              [VarParam])
-- The `c` function takes a variadic list, does not set the redex
prim_C :: MemAddr -> State -> [State]
prim_C refsMem state
  | Just (DataObj (RefsVal varMems) attrs)
      <- heapLookup refsMem $ stHeap state
  , Just vecs <- sequence $
      map (\m -> case heapLookup m $ stHeap state of
                    Just (DataObj (VecVal vec) _) -> Just vec
                    _ -> Nothing) varMems =
      let topTy = foldl (\accTy vecTy -> joinType accTy vecTy) BoolTy $
                        map vecType vecs in
      let topTyVecs = map ((flip vecToType) topTy) vecs in
      let mbConcatVec = case topTy of
            IntTy ->
              sequence (map vecInts topTyVecs) >>= Just . IntVec . concat
            DoubleTy ->
              sequence (map vecDoubles topTyVecs) >>= Just . DoubleVec . concat
            ComplexTy ->
              sequence (map vecComplexes topTyVecs) >>=
                Just . ComplexVec . concat
            StringTy ->
              sequence (map vecStrings topTyVecs) >>= Just . StringVec . concat
            BoolTy ->
              sequence (map vecBools topTyVecs) >>= Just . BoolVec . concat
            NullTy -> Just NilVec in
        case mbConcatVec of
          Just concatVec ->
            let vecObj = DataObj (VecVal concatVec) attrsEmpty in
            let (mem2, heap2) = heapAlloc vecObj $ stHeap state in
              [state { stRedex = ResultRed mem2
                     , stHeap = heap2 }]
          _ -> []

  | otherwise = []


--   , ("length",      RPrimLength,        [])
prim_Length :: MemAddr -> State -> [State]
prim_Length vecMem state
  | Just (DataObj (VecVal vec) attrs)
      <- heapLookup vecMem $ stHeap state =
      let lenConst = IntConst $ vecLength vec in
      let (mem, heap2) = heapAllocConst lenConst $ stHeap state in
        [state { stRedex = ResultRed mem
               , stHeap = heap2 }]

  | otherwise = []

--   , (":",           RPrimColon,          [])
prim_Colon :: MemAddr -> MemAddr -> State -> [State]
prim_Colon lowMem highMem state
  | Just (DataObj (VecVal (IntVec ((Atom low) : _))) attrsLow)
      <- heapLookup lowMem $ stHeap state
  , Just (DataObj (VecVal (IntVec ((Atom high) : _))) attrsHigh)
      <- heapLookup highMem $ stHeap state =
      let vec = IntVec $ map Atom [low .. high] in
      let obj = DataObj (VecVal vec) attrsEmpty in
      let (mem, heap2) = heapAlloc obj $ stHeap state in
        [state { stRedex = ResultRed mem
               , stHeap = heap2 }]

  | otherwise = []



