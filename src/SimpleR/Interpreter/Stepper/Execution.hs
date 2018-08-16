module SimpleR.Interpreter.Stepper.Execution
  ( RedResult (..)
  , RedAccum (..)
  , initRedAccMk
  , runN
  , runNHist
  , getValuesRunN
  , getValuesRed
  , ppHist
  , ppRedResult
  , ppRedAccum
  , ppRedAccumHist
  ) where

import Debug.Trace

import SimpleR.Interpreter.Commons
import SimpleR.Interpreter.Stepper.Strict

data RedResult =
    RedOkay Rule [State]
  | MultMatches [(Rule, [State])]
  | NoMatches

data RedAccum = RedAccum
  { compAcc :: [([Rule], State)]
  , incompAcc :: [([Rule], State)]
  , errorAcc :: [([Rule], State)]
  } deriving (Eq, Show, Read)

initRedAccMk :: State -> RedAccum
initRedAccMk state =
  RedAccum { compAcc = [], incompAcc = [([], state)], errorAcc = [] }

redAccEmpty :: RedAccum
redAccEmpty = RedAccum { compAcc = [], incompAcc = [], errorAcc = [] }

isStateDone :: State -> Bool
isStateDone state =
  case stRedex state of
    ResultRed _ -> stStack state == stackEmpty
    _ -> False

getStateVal :: State -> Maybe Value
getStateVal state
  | isStateDone state
  , ResultRed mem <- stRedex state
  , Just (DataObj val _) <- heapLookup mem $ stHeap state = Just val
  | otherwise = Nothing

tryRedsOnState :: State -> RedResult
tryRedsOnState state =
  case filter ((/= []) . snd) $ map (\(r, f) -> (r, f state)) rulePairs of
    [] -> NoMatches
    ((r, ss) : []) -> RedOkay r ss
    toomuch -> MultMatches toomuch

tryRedsOnStateHist :: [Rule] -> State -> RedAccum
tryRedsOnStateHist rules state =
  case tryRedsOnState state of
    RedOkay r ss ->
      let comps = filter isStateDone ss in
      let incomps = filter (not . isStateDone) ss in
        redAccEmpty
          { compAcc = map (\s -> (rules ++ [r], s)) comps
          , incompAcc = map (\s -> (rules ++ [r], s)) incomps }
    MultMatches ress ->
      trace ("runRed: multiple matches on state:\n" ++ ppState state) $
            redAccEmpty
              { errorAcc =
                concatMap (\(r, ss) ->
                            map (\s -> (rules ++ [r], s)) ss) ress }
    NoMatches ->
      trace ("runRed: no matches on state:\n" ++ ppState state) $
            redAccEmpty { errorAcc = [(rules, state)] }

mergeRedAccums :: [RedAccum] -> RedAccum
mergeRedAccums reds =
  foldl (\acc red ->
              acc { compAcc = (compAcc acc) ++ (compAcc red)
                  , incompAcc = (incompAcc acc) ++ (incompAcc red)
                  , errorAcc = (errorAcc acc) ++ (errorAcc red) })
        redAccEmpty reds

runRedsOnIncomps :: [([Rule], State)] -> RedAccum
runRedsOnIncomps incomps =
  mergeRedAccums $ map (uncurry tryRedsOnStateHist) incomps

runRedsOnRedAccum :: RedAccum -> RedAccum
runRedsOnRedAccum acc =
  let redded = runRedsOnIncomps $ incompAcc acc in
    acc { compAcc = (compAcc acc) ++ (compAcc redded)
        , incompAcc = incompAcc redded
        , errorAcc = (errorAcc acc) ++ (errorAcc redded) }

runN :: Int -> State -> RedAccum
runN n state =
  foldr (.) id (replicate n runRedsOnRedAccum) $ initRedAccMk state

runNHist :: Int -> State -> [RedAccum]
runNHist n state = take n $ iterate runRedsOnRedAccum $ initRedAccMk state

getValuesRunN :: Int -> State -> [Maybe Value]
getValuesRunN n state = map (getStateVal . snd) $ compAcc $ runN n state

getValuesRed :: RedAccum -> [Maybe Value]
getValuesRed redAcc = map (getStateVal . snd) $ compAcc redAcc

----
-- A few printing functions

ppHist :: ([Rule], State) -> String
ppHist (rules, state) =
  -- ppState state
  -- show rules
  show rules ++ "\n" ++ ppState state

ppRedResult :: RedResult -> String
ppRedResult NoMatches = ">>> NoMatches"
ppRedResult (RedOkay rule states) =
  ">>> RedOkay\n" ++
  (injBreak "---" $
    map (\s -> ppHist ([rule], s)) states)
ppRedResult (MultMatches pairs) =
  ">>> MulMatches\n" ++
  (injBreak "-----" $
    map (\(r, ss) -> injBreak "---" $
                      map (\s -> ppHist ([r], s)) ss) pairs)

ppRedAccum :: RedAccum -> String
ppRedAccum acc =
  "RedAccum\n" ++
  ">> Completes\n" ++ (concatMap ppHist $ compAcc acc) ++ "\n" ++
  ">> Completes\n" ++ (injBreak "---" $ map ppHist $ compAcc acc) ++ "\n" ++
  ">> Incompletes\n" ++ (injBreak "---" $ map ppHist $ incompAcc acc) ++ "\n" ++
  ">> Errors\n" ++ (injBreak "---" $ map ppHist $ errorAcc acc)

ppRedAccumHist :: [RedAccum] -> String
ppRedAccumHist accs =
  injBreak "*************" $ map ppRedAccum accs


