module SimpleR.Interpreter.Stepper.ParamArgMatching where

import SimpleR.Language
import SimpleR.Interpreter.Commons

-- Extract default (Ident, Expr) bindings from a [Param]
bindsOfDefaults :: [Param] -> [(Ident, Expr)]
bindsOfDefaults [] = []
bindsOfDefaults ((Param _) : ps) = bindsOfDefaults ps
bindsOfDefaults (VarParam : ps) = bindsOfDefaults ps
bindsOfDefaults ((Default id expr) : ps) = (id, expr) : bindsOfDefaults ps

-- Linearize the arguments into a [Either MemRef (Ident, MemRef)] if possible.
-- Flattens the variadics.
pullArgs :: [(Arg, MemRef)] -> Heap -> Maybe [Either MemRef (Ident, MemRef)]
pullArgs [] _ = Just []
pullArgs (arg : args) heap = do
  args2 <- pullArgs args heap
  case arg of
    (Arg _, mem) -> return $ (Left mem) : args2
    (Named id expr, mem) -> return $ (Right (id, mem)) : args2
    (VarArg, mem) -> do
      (DataObj (RefsVal varMems) attrs) <- heapLookup mem heap
      nameMem <- attrsLookup "name" attrs
      (DataObj (VecVal (StringVec nameStrs)) _) <- heapLookup nameMem heap
      if length nameStrs == length varMems then
        let strMemPairs = zip nameStrs varMems in
        let idMemPairs =
              map (\(n, m) -> if n == ""
                    then Left m
                    else Right (idFromString n, m)) strMemPairs in
          return $ idMemPairs ++ args2
      else
        Nothing

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
-- the second element, consisting of [Either MemRef (Ident, MemRef)]
-- The accumulator contains a triple of
--   [Param] -- the parameters still unmatched
--   [Either MemRef (Ident, MemRef)] -- args to be positionally matched
--   [(Ident, MemRef)] -- The default argument pairings
defMatchFoldL ::
  ([Param], [Either MemRef (Ident, MemRef)], [(Ident, MemRef)]) ->
  (Either MemRef (Ident, MemRef)) ->
    ([Param], [Either MemRef (Ident, MemRef)], [(Ident, MemRef)])
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
--  [Either MemRef (Ident, MemRef)] -- the arguments
positionalMatch ::
  [Param] ->
  [Either MemRef (Ident, MemRef)] ->
    ([(Ident, Either MemRef Expr)],
     [Either MemRef (Ident, MemRef)])
-- In the case where we have no arguments, just discard everything
positionalMatch [] _ = ([], [])
-- When we have only parameters, we must extract the default values
positionalMatch params [] =
  let defaults = bindsOfDefaults params in
  let binds = map (\(i, e) -> (i, Right e)) defaults in
    (binds, [])
-- When we have variadic parameter, can only hope to get tails of param
positionalMatch (VarParam : params) _ =
  let defaults = bindsOfDefaults params in
  let binds = map (\(i, e) -> (i, Right e)) defaults in
    (binds, [])
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
  [Param] -> [(Arg, MemRef)] -> Env -> Heap ->
    Maybe ([(Ident, Either MemRef Expr)],
           [Either MemRef (Ident, MemRef)])
matchLamApp params args env heap = do
  pulled <- pullArgs args heap
  let acc = (params, [], [])
  let (remParams, remArgs, namedArgs) = foldl defMatchFoldL acc pulled
  let (matched, vars) = positionalMatch remParams remArgs
  let binds = map (\(i, m) -> (i, Left m)) namedArgs
  return (binds, vars)


