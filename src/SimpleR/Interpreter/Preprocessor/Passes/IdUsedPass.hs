module SimpleR.Interpreter.Preprocessor.Passes.IdUsedPass
  ( idUsedPass
  ) where

import SimpleR.Language
import SimpleR.Interpreter.Preprocessor.Passes.PassCommons

-- We want to reject programs that uses a specific id.
-- This is overly conservative because we also go through args and params

idUsedArg :: Ident -> Arg -> PassResult Arg
idUsedArg i (Arg expr) = idUsedExpr i expr >>= PassOkay . Arg
idUsedArg i (Named id expr) = do
  if i == id then
    PassFail ["idUsedArg: detected " ++ show id]
  else
    idUsedExpr i expr >>= PassOkay . Named id
idUsedArg _ (VarArg) = PassOkay VarArg

idUsedParam :: Ident -> Param -> PassResult Param
idUsedParam i (Param id) =
  if i == id then
    PassFail ["idUsedParam: detected " ++ show id]
  else
    PassOkay $ Param id
idUsedParam i (Default id expr) =
  if i == id then
    PassFail ["idUsedParam: detected " ++ show id]
  else
    PassOkay $ Default id expr
idUsedParam _ (VarParam) = PassOkay VarParam

idUsedExpr :: Ident -> Expr -> PassResult Expr
idUsedExpr i (Var id) =
  if i == id then
    PassFail ["idUsedExpr: detected " ++ show id]
  else
    PassOkay $ Var id
idUsedExpr _ (Const const) = PassOkay $ Const const
idUsedExpr i (Seq exprs) = mapM (idUsedExpr i) exprs >>= PassOkay . Seq
idUsedExpr i (LamAbs params expr) = do
  ps <- mapM (idUsedParam i) params
  e <- idUsedExpr i expr
  return $ LamAbs ps e
idUsedExpr i (LamApp (Var f) args) = do
  if i == f then
    PassFail ["idUsedExpr: detected " ++ show f]
  else do
    as <- mapM (idUsedArg i) args
    return $ LamApp (Var f) as
idUsedExpr i (LamApp fun args) = do
  as <- mapM (idUsedArg i) args
  f <- idUsedExpr i fun
  return $ LamApp f as
idUsedExpr i (NativeLamApp id ids) = do
  if i `elem` (id : ids) then
    PassFail ["idUsedExpr: detected " ++ show i]
  else
    PassOkay $ NativeLamApp id ids
idUsedExpr i (Return expr) = idUsedExpr i expr >>= PassOkay . Return
idUsedExpr i (Assign expr1 expr2) = do
  e1 <- idUsedExpr i expr1
  e2 <- idUsedExpr i expr2
  return $ Assign e1 e2
idUsedExpr i (If expr1 expr2 expr3) = do
  e1 <- idUsedExpr i expr1
  e2 <- idUsedExpr i expr2
  e3 <- idUsedExpr i expr3
  return $ If e1 e2 e3
idUsedExpr i (While exprc exprb) = do
  ec <- idUsedExpr i exprc
  eb <- idUsedExpr i exprb
  return $ While ec eb
idUsedExpr i (Break) = PassOkay Break
idUsedExpr i (Next) = PassOkay Next
idUsedExpr i (Error) = PassOkay Error -- Is this fine?
idUsedExpr i (Tix tick expr) = idUsedExpr i expr >>= PassOkay . Tix tick

idUsedPass :: Ident -> Program -> PassResult Program
idUsedPass id (Program exprs) =
  mapM (idUsedExpr id) exprs >>= PassOkay . Program


