module SimpleR.Interpreter.Natives.PrimNumLogOps

  where

import Control.Applicative

import Debug.Trace

import SimpleR.Language
import SimpleR.Interpreter.Commons
import SimpleR.Interpreter.Natives.RPrimitives
import SimpleR.Smt

-- Implicitly handle all these things as doubles????

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

data NumBinOp =
  NumBinOp
    { vNumBopPrim :: RPrim
    , vNumBopIntFun :: Int -> Int -> Int
    , vNumBopDoubleFun :: Double -> Double -> Double
    , vNumBopComplexFun :: Complex -> Complex -> Complex
    , vNumBopBoolFun :: Bool -> Bool -> Int
    }

data LogBinOp =
  LogBinOp
    { vLogBopPrim :: RPrim
    , vLogBopIntFun :: Int -> Int -> Bool
    , vLogBopDoubleFun :: Double -> Double -> Bool
    , vLogBopComplexFun :: Complex -> Complex -> Bool
    , vLogBopBoolFun :: Bool -> Bool -> Bool
    , vLogBopStringFun :: String -> String -> Bool
    }

data VecBinOp = NumVecBinOp NumBinOp | LogVecBinOp LogBinOp

boolIntFun :: (Bool -> Bool -> Bool) -> Bool -> Bool -> Int
boolIntFun f a b = if f a b then 1 else 0

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

noExistError :: String -> a
noExistError opName = error $ opName ++ ": does not exist"

plusBinOp :: VecBinOp
plusBinOp = NumVecBinOp $ NumBinOp
  RPrimPlus
  (+)
  (+)
  (+)
  (\a b -> boolToInt a + boolToInt b)

subBinOp :: VecBinOp
subBinOp = NumVecBinOp $ NumBinOp
  RPrimMinus
  (-)
  (-)
  (-)
  (\a b -> boolToInt a - boolToInt b)

multBinOp :: VecBinOp
multBinOp = NumVecBinOp $ NumBinOp
  RPrimMult
  (*)
  (*)
  (*)
  (\a b -> boolToInt a * boolToInt b)
  
divBinOp :: VecBinOp
divBinOp = NumVecBinOp $ NumBinOp
  RPrimDiv
  (noExistError "divBinOp")
  (/)
  (/)
  (noExistError "divBinOp")

powBinOp :: VecBinOp
powBinOp = NumVecBinOp $ NumBinOp
  RPrimPow
  (^)
  (noExistError "powBinOp")
  (noExistError "powBinOp")
  (\a b -> boolToInt a ^ boolToInt b)

modBinOp :: VecBinOp
modBinOp = NumVecBinOp $ NumBinOp
  RPrimMod
  (mod)
  (noExistError "modBinOp")
  (noExistError "modBinOp")
  (noExistError "modBinOp")
  -- (\a b -> boolToInt a `mod` boolToInt b)

andBinOp :: VecBinOp
andBinOp = LogVecBinOp $ LogBinOp
  RPrimAnd
  (\a b -> a /= 0 && b /= 0)
  (\a b -> a /= 0 && b /= 0)
  (\a b -> a /= 0 && b /= 0)
  (&&)
  (noExistError "andBinOp")

orBinOp :: VecBinOp
orBinOp = LogVecBinOp $ LogBinOp
  RPrimOr
  (\a b -> a /= 0 || b /= 0)
  (\a b -> a /= 0 || b /= 0)
  (\a b -> a /= 0 || b /= 0)
  (||)
  (noExistError "orBinOp")

eqBinOp :: VecBinOp
eqBinOp = LogVecBinOp $ LogBinOp
  RPrimEq
  (==)
  (==)
  (==)
  (==)
  (==)

neqBinOp :: VecBinOp
neqBinOp = LogVecBinOp $ LogBinOp
  RPrimNeq
  (/=)
  (/=)
  (/=)
  (/=)
  (/=)

ltBinOp :: VecBinOp
ltBinOp = LogVecBinOp $ LogBinOp
  RPrimLt
  (<)
  (<)
  (noExistError "ltBinOp") -- Complex
  (<)
  (<)

leBinOp :: VecBinOp
leBinOp = LogVecBinOp $ LogBinOp
  RPrimLe
  (<=)
  (<=)
  (noExistError "leBinOp") -- Complex
  (<=)
  (<=)

geBinOp :: VecBinOp
geBinOp = LogVecBinOp $ LogBinOp
  RPrimGe
  (>=)
  (>=)
  (noExistError "geBinOp") -- Complex
  (>=)
  (>=)

gtBinOp :: VecBinOp
gtBinOp = LogVecBinOp $ LogBinOp
  RPrimGt
  (>)
  (>)
  (noExistError "gtBinOp") -- Complex
  (>)
  (>)

applyNumBinOp ::
  NumBinOp -> Vector -> Vector -> Either Vector ([SmtExpr], [SmtCmd])
applyNumBinOp binop vec1 vec2 =
  case (vec1, vec2) of
    (SymVec sid1 sty1 slen1, SymVec sid2 sty2 slen2) -> undefined
    (SymVec sid1 sty1 slen1, _) -> undefined
    (_, SymVec sid2 sty2 slen2) -> undefined

    _ ->
      case vecPairLengthenJoinType (vec1, vec2) of
        (IntVec xs1, IntVec xs2) ->
          Left $ IntVec
               $ map (uncurry $ liftA2 $ vNumBopIntFun binop) $ zip xs1 xs2
        (DoubleVec xs1, DoubleVec xs2) ->
          Left $ DoubleVec
               $ map (uncurry $ liftA2 $ vNumBopDoubleFun binop) $ zip xs1 xs2
        (ComplexVec xs1, ComplexVec xs2) ->
          Left $ ComplexVec
               $ map (uncurry $ liftA2 $ vNumBopComplexFun binop) $ zip xs1 xs2
        (BoolVec xs1, BoolVec xs2) ->
          Left $ IntVec
               $ map (uncurry $ liftA2 $ vNumBopBoolFun binop) $ zip xs1 xs2
        _ -> error $ "applyNumBinOp: vecPairLengthenJoinType failed"

applyLogBinOp ::
  LogBinOp -> Vector -> Vector -> Either Vector ([SmtExpr], [SmtCmd])
applyLogBinOp binop vec1 vec2 =
  case (vec1, vec2) of
    (SymVec sid1 sty1 slen1, SymVec sid2 sty2 slen2) -> undefined
    (SymVec sid1 sty1 slen1, _) -> undefined
    (_, SymVec sid2 sty2 slen2) -> undefined
    _ -> 
      case vecPairLengthenJoinType (vec1, vec2) of
        (IntVec xs1, IntVec xs2) ->
          Left $ BoolVec
               $ map (uncurry $ liftA2 $ vLogBopIntFun binop) $ zip xs1 xs2
        (DoubleVec xs1, DoubleVec xs2) ->
          Left $ BoolVec
               $ map (uncurry $ liftA2 $ vLogBopDoubleFun binop) $ zip xs1 xs2
        (ComplexVec xs1, ComplexVec xs2) ->
          Left $ BoolVec
               $ map (uncurry $ liftA2 $ vLogBopComplexFun binop) $ zip xs1 xs2
        (BoolVec xs1, BoolVec xs2) ->
          Left $ BoolVec
               $ map (uncurry $ liftA2 $ vLogBopBoolFun binop) $ zip xs1 xs2
        (StringVec xs1, StringVec xs2) ->
          Left $ BoolVec
               $ map (uncurry $ liftA2 $ vLogBopStringFun binop) $ zip xs1 xs2

applyVecBinOp ::
  VecBinOp -> Vector -> Vector -> Either Vector ([SmtExpr], [SmtCmd])
applyVecBinOp (NumVecBinOp binop) vec1 vec2 = applyNumBinOp binop vec1 vec2
applyVecBinOp (LogVecBinOp binop) vec1 vec2 = applyLogBinOp binop vec1 vec2

prim_BinOp ::
  MemAddr -> MemAddr -> VecBinOp -> State -> [State]
prim_BinOp vecMem1 vecMem2 binop state
  | Just (DataObj (VecVal (SymVec sid1 sty1 slen1)) attrs1)
      <- heapLookup vecMem1 $ stHeap state
  , Just (DataObj (VecVal (SymVec sid2 sty2 slen2)) attrs2)
      <- heapLookup vecMem2 $ stHeap state =
      undefined

  | Just (DataObj (VecVal (SymVec sid1 sty1 slen1)) attrs1)
      <- heapLookup vecMem1 $ stHeap state
  , Just (DataObj (VecVal vec2) attrs2) <- heapLookup vecMem2 $ stHeap state =
      undefined

  | Just (DataObj (VecVal vec1) attrs1) <- heapLookup vecMem1 $ stHeap state
  , Just (DataObj (VecVal (SymVec sid2 sty2 slen2)) attrs2)
      <- heapLookup vecMem2 $ stHeap state =
      undefined

  | Just (DataObj (VecVal vec1) attrs1) <- heapLookup vecMem1 $ stHeap state
  , Just (DataObj (VecVal vec2) attrs2) <- heapLookup vecMem2 $ stHeap state =
      case applyVecBinOp binop vec1 vec2 of
        Left vec3 ->
          -- Take on the first dimension?
          let obj = DataObj (VecVal vec3) attrs1 in
          let (mem, heap2) = heapAlloc obj $ stHeap state in
            [state { stRedex = ResultRed mem
                   , stHeap = heap2 }]
        _ -> error $ "prim_BinOp: failed for concretes " ++ show (vec1, vec2)


-- Some primitive things we just throw in here

-- Everything becomes a bool
notVector :: Vector -> Vector
notVector (IntVec xs) = BoolVec $ map (fmap (/= 0)) xs
notVector (DoubleVec xs) = BoolVec $ map (fmap (/= 0)) xs
notVector (ComplexVec xs) = BoolVec $ map (fmap (/= 0)) xs
notVector (BoolVec xs) = BoolVec $ map (fmap not) xs
notVector vec = error $ "notVector: wrong stuff " ++ show vec

prim_UNotOp :: MemAddr -> State -> [State]
prim_UNotOp vecMem state
  | Just (DataObj (VecVal (SymVec sid sty slen)) attrs)
      <- heapLookup vecMem $ stHeap state = undefined
  
  | Just (DataObj (VecVal vec) attrs)
      <- heapLookup vecMem $ stHeap state =
        let notVec = notVector vec in
        let obj = DataObj (VecVal notVec) attrs in
        let (mem, heap2) = heapAlloc obj $ stHeap state in
          [state { stRedex = ResultRed mem
                 , stHeap = heap2 }]

  | otherwise = []

-- Bools become an int
negVector :: Vector -> Vector
negVector (IntVec xs) = IntVec $ map (fmap (* (-1))) xs
negVector (DoubleVec xs) = DoubleVec $ map (fmap (* (-1))) xs
negVector (ComplexVec xs) = ComplexVec $ map (fmap (* (-1))) xs
negVector (BoolVec xs) = IntVec $ map (fmap ((* (-1)) . boolToInt)) xs
negVector vec = error $ "negVector: wrong stuff " ++ show vec

prim_UNegOp :: MemAddr -> State -> [State]
prim_UNegOp vecMem state
  | Just (DataObj (VecVal (SymVec sid sty slen)) attrs)
      <- heapLookup vecMem $ stHeap state = undefined
  
  | Just (DataObj (VecVal vec) attrs)
      <- heapLookup vecMem $ stHeap state =
        let negVec = negVector vec in
        let obj = DataObj (VecVal negVec) attrs in
        let (mem, heap2) = heapAlloc obj $ stHeap state in
          [state { stRedex = ResultRed mem
                 , stHeap = heap2 }]

  | otherwise = []

-- This doens't seem to do much??
prim_UPlus :: MemAddr -> State -> [State]
prim_UPlus vecMem state
  | Just (DataObj (VecVal (SymVec sid sty slen)) attrs)
      <- heapLookup vecMem $ stHeap state = undefined

  | Just (DataObj (VecVal vec) attrs)
      <- heapLookup vecMem $ stHeap state =
        let vec1 = vec in
        let obj = DataObj (VecVal vec1) attrs in
        let (mem, heap2) = heapAlloc obj $ stHeap state in
          [state { stRedex = ResultRed mem
                 , stHeap = heap2 }]

  | otherwise = []


