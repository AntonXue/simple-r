module SimpleR.Interpreter.Natives.PrimBinOps

  where

import Debug.Trace

import SimpleR.Language
import SimpleR.Interpreter.Commons
import SimpleR.Smt

--   -- More Numerical
--   , ("+",   RPrimPlus,    [])
--   , ("-",   RPrimMinus,   [])
--   , ("*",   RPrimMult,    [])
--   , ("/",   RPrimDiv,     [])
--   , ("^",   RPrimPow,     [])
--   , ("%%",  RPrimMod,     [])
--   , ("%/%", RPrimIntDiv,  [])
--   , ("&",   RPrimAnd,     [])
--   , ("|",   RPrimOr,      [])
--   , ("==",  RPrimEq,      [])
--   , ("!=",  RPrimNeq,     [])
--   , ("<",   RPrimLt,      [])
--   , ("<=",  RPrimLe,      [])
--   , (">=",  RPrimGe,      [])
--   , (">",   RPrimGt,      [])

data SmtBinOp =
    CompOp (SmtExpr -> SmtExpr -> SmtCompExpr)
  | LogicOp (SmtExpr -> SmtExpr -> SmtLogicExpr)
  | MathOp (SmtExpr -> SmtExpr -> SmtMathExpr)

data VecBinOp = VecBinOp
  { intBinOp :: Int -> Int -> Int
  , doubleBinOp :: Double -> Double -> Double
  , complexBinOp :: Complex -> Complex -> Complex
  , stringBinOp :: String -> String -> String
  , boolBinOp :: Bool -> Bool -> Bool
  , symBinOp :: SmtIdent -> SmtIdent -> SmtBinOp
  , floorType :: Type
  , ceilType :: Type
  }

atomZip :: [Atom a] -> [Atom b] -> [Atom (a, b)]
atomZip axs ays =
  map (\(ax, ay) -> do { x <- ax; y <- ay; Atom (x, y) }) $ zip axs ays

isVecSym :: Vector -> Bool
isVecSym (SymVec _ _) = True
isVecSym _ = False

applyBinOp :: VecBinOp -> MemRef -> MemRef -> State ->
  Maybe (MemRef, State)
applyBinOp op vecMem1 vecMem2 state
  | Just (DataObj (VecVal vec1) attrs1) <- heapLookup vecMem1 $ stHeap state
  , Just (DataObj (VecVal vec2) attrs2) <- heapLookup vecMem2 $ stHeap state
  , attrs1 == attrsEmpty
  , attrs2 == attrsEmpty
  , not $ isVecSym vec1
  , not $ isVecSym vec2 = do
      let (vec1, vec2) = vecPairLengthen $ vecPairJoinType (vec1, vec2)
      resVec <- case (vec1, vec2) of
        (IntVec xs1, IntVec xs2) ->
          let aOp = fmap (uncurry $ intBinOp op) in
            Just $ IntVec $ map aOp $ atomZip xs1 xs2
        (DoubleVec xs1, DoubleVec xs2) ->
          let aOp = fmap (uncurry $ doubleBinOp op) in
            Just $ DoubleVec $ map aOp $ atomZip xs1 xs2
        (ComplexVec xs1, ComplexVec xs2) ->
          let aOp = fmap (uncurry $ complexBinOp op) in
            Just $ ComplexVec $ map aOp $ atomZip xs1 xs2
        (StringVec xs1, StringVec xs2) ->
          let aOp = fmap (uncurry $ stringBinOp op) in
            Just $ StringVec $ map aOp $ atomZip xs1 xs2
        (BoolVec xs1, BoolVec xs2) ->
          let aOp = fmap (uncurry $ boolBinOp op) in
            Just $ BoolVec $ map aOp $ atomZip xs1 xs2
        _ ->
          trace ("applyBinOp: failed to convert concrete vectors") Nothing
      let obj = DataObj (VecVal resVec) attrsEmpty
      let (mem, heap2) = heapAlloc obj $ stHeap state
      return $ (mem, state { stHeap = heap2 })
  
  | Just (DataObj (VecVal (SymVec sym1 ty1)) attrs1)
      <- heapLookup vecMem1 $ stHeap state
  , Just (DataObj (VecVal (SymVec sym2 ty2)) attrs2)
      <- heapLookup vecMem2 $ stHeap state
  , attrs1 == attrsEmpty
  , attrs2 == attrsEmpty = do
      undefined

  | otherwise = Nothing

vecBinOpMk ::
  (Int -> Int -> Int) ->
  (Double -> Double -> Double) ->
  (Complex -> Complex -> Complex) ->
  (String -> String -> String) ->
  (Bool -> Bool -> Bool) ->
  (SmtIdent -> SmtIdent -> SmtBinOp) ->
  Type ->
  Type ->
  VecBinOp
vecBinOpMk fInt fDouble fComplex fString fBool fSym fTy cTy =
  VecBinOp { intBinOp = fInt
           , doubleBinOp = fDouble
           , complexBinOp = fComplex
           , stringBinOp = fString
           , boolBinOp = fBool
           , symBinOp = fSym
           , floorType = fTy
           , ceilType = cTy }

plusBinOp :: VecBinOp
plusBinOp =
  vecBinOpMk
    (+)
    (+)
    (+)
    (error "plusBinOp: no string")
    (error "plusBinOp: no bool")
    (error "plusBinOp: no sym")
    IntTy
    ComplexTy

minusBinOp :: VecBinOp
minusBinOp =
  vecBinOpMk
    (-)
    (-)
    (-)
    (error "minusBinOp: no string")
    (error "minusBinOp: no bool")
    (error "minusBinOp: no sym")
    IntTy
    ComplexTy

multBinOp :: VecBinOp
multBinOp =
  vecBinOpMk
    (*)
    (*)
    (*)
    (error "multBinOp: no string")
    (error "multBinOp: no bool")
    (error "multBinOp: no sym")
    IntTy
    ComplexTy

divBinOp :: VecBinOp
divBinOp =
  vecBinOpMk
    (error "divBinOp: no div")
    (/)
    (/)
    (error "divBinOp: no string")
    (error "divBinOp: no bool")
    (error "divBinOp: no sym")
    DoubleTy
    ComplexTy

modBinOp :: VecBinOp
modBinOp =
  vecBinOpMk
    (mod)
    (error "modBinOp: no double")
    (error "modBinOp: no complex")
    (error "modBinOp: no string")
    (error "modBinOp: no bool")
    (error "modBinOp: no sym")
    IntTy
    IntTy



