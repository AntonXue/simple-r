module SimpleR.Interpreter.Stepper.ParamArgMatching where

import Debug.Trace

import SimpleR.Language
import SimpleR.Interpreter.Commons

-- Extract default (Ident, Expr) bindings from a [Param]
bindsOfDefaults :: [Param] -> [(Ident, Expr)]
bindsOfDefaults [] = []
bindsOfDefaults ((Param _) : ps) = bindsOfDefaults ps
bindsOfDefaults (VarParam : ps) = bindsOfDefaults ps
bindsOfDefaults ((Default id expr) : ps) = (id, expr) : bindsOfDefaults ps

-- Linearize the arguments into a [Either MemAddr (Ident, MemAddr)] if possible.
-- Flattens the variadics.
pullArgs :: [(Arg, MemAddr)] -> Heap -> Maybe [Either MemAddr (Ident, MemAddr)]
pullArgs [] _ = Just []
pullArgs ((Arg _, mem) : args) heap
  | Just args2 <- pullArgs args heap = Just $ (Left mem) : args2
pullArgs ((Named id expr, mem) : args) heap
  | Just args2 <- pullArgs args heap = Just $ (Right (id, mem)) : args2
pullArgs ((VarArg, mem) : args) heap
  | Just args2 <- pullArgs args heap
  , Just (DataObj (RefsVal varMems) attrs) <- heapLookup mem heap
  , Just nameMem <- attrsLookup idAttrsNames attrs
  , Just (DataObj (VecVal (StringVec nameSStrs)) _) <- heapLookup nameMem heap
  , length nameSStrs == length varMems =
      let strMemPairs = zip nameSStrs varMems in
      let idMemPairs =
            map (\(n, m) ->
                  if n == Atom "" then
                    Left m
                  else
                    Right (idFromAtomString n, m)) strMemPairs in
        Just $ idMemPairs ++ args2
  | otherwise = Nothing

-- Remove the param that is used based on the Ident.
dropUsed :: Ident -> [Param] -> [Param]
dropUsed id params =
  filter
    (\p -> case p of
      Param pid -> id /= pid
      Default pid _ -> id /= pid
      VarParam -> True)
    params

dropUseds :: [Ident] -> [Param] -> [Param]
dropUseds [] params = params
dropUseds (id : ids) params = dropUseds ids $ dropUsed id params

-- Intended as the function for foldl over matching the [Param] to the
-- pulled arguments; quite, hacky, yes!
-- The idea is that we go through a list of arguments, and gradually
-- filter out the [Param] in the first element of the accumulator triple,
-- which is then left to be positionally matched with
-- the second element, consisting of [Either MemAddr (Ident, MemAddr)]
-- The accumulator contains a triple of
--   [Param] -- the parameters still unmatched
--   [Either MemAddr (Ident, MemAddr)] -- args to be positionally matched
--   [(Ident, MemAddr)] -- The default argument pairings
defMatchFoldL ::
  ([Param], [Either MemAddr (Ident, MemAddr)], [(Ident, MemAddr)]) ->
  (Either MemAddr (Ident, MemAddr)) ->
    ([Param], [Either MemAddr (Ident, MemAddr)], [(Ident, MemAddr)])
defMatchFoldL (params, posArgs, namedArgs) arg =
  case arg of
    Left mem -> (params, posArgs ++ [arg], namedArgs)
    Right (id, mem) ->
      let filtParams = dropUsed id params in
        -- Managed to have named binding match
        if length filtParams < length params then
          (filtParams, posArgs, namedArgs ++ [(id, mem)])
        -- Unsuccessful and accomplished nothing
        else
          (filtParams, posArgs ++ [arg], namedArgs)

-- Match the remaining parameters and arguments.
-- Also detect the variadic match we output.
-- Outputs (the matched positional arguments, variadic arguments with naming)
--  [Param] -- the parameters
--  [Either MemAddr (Ident, MemAddr)] -- the arguments
positionalMatch ::
  [Param] ->
  [Either MemAddr (Ident, MemAddr)] ->
    ([(Ident, Either MemAddr Expr)],
     [Either MemAddr (Ident, MemAddr)])
-- In the case where we have no arguments, just discard everything
positionalMatch [] _ = ([], [])
-- When we have only parameters, we must extract the default values
positionalMatch params [] =
  let defaults = bindsOfDefaults params in
  let binds = map (\(i, e) -> (i, Right e)) defaults in
    (binds, [])
-- When we have variadic parameter, can only hope to get tails of param
positionalMatch (VarParam : params) args =
  let defaults = bindsOfDefaults params in
  let binds = map (\(i, e) -> (i, Right e)) defaults in
    (binds, args)
-- Regular paramters match non-named arguments (should be filtered earlier!)
positionalMatch (Param pId : params) (arg : args) =
  case arg of
    -- Argument is named, which should have been discarded! Ignore for now.
    Right (_, _) -> positionalMatch params args
    -- Argument is regular memory, great!
    Left mem ->
      let (binds, vars) = positionalMatch params args in
        ((pId, Left mem) : binds, vars)
-- Default parameter gets overwritten by value in its position
positionalMatch ((Default pId _) : params) (arg : args) =
  case arg of
    -- Like before, all named parameters should be gone by now.
    Right (_, _) -> positionalMatch params args
    Left mem ->
      let (binds, vars) = positionalMatch params args in
        ((pId, Left mem) : binds, vars)

matchLamApp ::
  [Param] -> [(Arg, MemAddr)] -> Env -> Heap ->
    Maybe ([(Ident, Either MemAddr Expr)],
           [Either MemAddr (Ident, MemAddr)])
matchLamApp params args env heap = do
  pulled <- pullArgs args heap
  let acc = (params, [], [])
  let (remParams, remArgs, namedArgs) = foldl defMatchFoldL acc pulled
  let (matched, vars) = positionalMatch remParams remArgs
  let binds = map (\(i, m) -> (i, Left m)) namedArgs
  return (binds ++ matched, vars)


