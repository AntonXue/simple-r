module SimpleR.Interpreter.Preprocessor.Passes.FunCalledPass
  ( funCalledPass
  ) where

import SimpleR.Language
import SimpleR.Interpreter.Preprocessor.Passes.PassCommons

-- We want to reject programs that make a function call with a specific id.
-- I think this is broken, since first-class functional values exist in R,
-- but try it anyways.

funUsedArg :: Ident -> Arg -> PassResult Arg
funUsedArg i (Arg expr) = funUsedExpr i expr >>= PassOkay . Arg
funUsedArg i (Named id expr) = funUsedExpr i expr >>= PassOkay . Named id
funUsedArg _ (VarArg) = PassOkay VarArg

funUsedParam :: Ident -> Param -> PassResult Param
funUsedParam _ (Param id) = PassOkay $ Param id
funUsedParam i (Default id expr) = funUsedExpr i expr >>= PassOkay . Default id
funUsedParam _ (VarParam) = PassOkay VarParam

funUsedExpr :: Ident -> Expr -> PassResult Expr
funUsedExpr _ (Var id) = PassOkay $ Var id
funUsedExpr _ (Const const) = PassOkay $ Const const
funUsedExpr i (Seq exprs) = mapM (funUsedExpr i) exprs >>= PassOkay . Seq
funUsedExpr i (LamAbs params expr) = do
  ps <- mapM (funUsedParam i) params
  e <- funUsedExpr i expr
  return $ LamAbs ps e
funUsedExpr i (LamApp (Var f) args) = do
  if i == f then
    PassFail ["funUsedExpr: detected " ++ show f]
  else do
    as <- mapM (funUsedArg i) args
    return $ LamApp (Var f) as
funUsedExpr i (LamApp fun args) = do
  as <- mapM (funUsedArg i) args
  f <- funUsedExpr i fun
  return $ LamApp f as
funUsedExpr i (NativeLamApp id ids) = PassOkay $ NativeLamApp id ids
funUsedExpr i (Return expr) = funUsedExpr i expr >>= PassOkay . Return
funUsedExpr i (Assign expr1 expr2) = do
  e1 <- funUsedExpr i expr1
  e2 <- funUsedExpr i expr2
  return $ Assign e1 e2
funUsedExpr i (If expr1 expr2 expr3) = do
  e1 <- funUsedExpr i expr1
  e2 <- funUsedExpr i expr2
  e3 <- funUsedExpr i expr3
  return $ If e1 e2 e3
funUsedExpr i (While exprc exprb) = do
  ec <- funUsedExpr i exprc
  eb <- funUsedExpr i exprb
  return $ While ec eb
funUsedExpr i (Break) = PassOkay Break
funUsedExpr i (Next) = PassOkay Next
funUsedExpr i (Error) = PassOkay Error -- Is this fine?
funUsedExpr i (Tix tick expr) = funUsedExpr i expr >>= PassOkay . Tix tick


funCalledPass :: Ident -> Program -> PassResult Program
funCalledPass id (Program exprs) =
  mapM (funUsedExpr id) exprs >>= PassOkay . Program


