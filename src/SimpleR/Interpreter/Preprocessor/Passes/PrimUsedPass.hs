module SimpleR.Interpreter.Preprocessor.Passes.PrimUsedPass
  ( primUsedPass
  ) where

import SimpleR.Language
import SimpleR.Interpreter.Preprocessor.Passes.PassCommons

-- Does a specific primitive (native) function get called?

primUsedArg :: Ident -> Arg -> PassResult Arg
primUsedArg i (Arg expr) = primUsedExpr i expr >>= PassOkay . Arg
primUsedArg i (Named id expr) = primUsedExpr i expr >>= PassOkay . Named id
primUsedArg _ (VarArg) = PassOkay VarArg

primUsedParam :: Ident -> Param -> PassResult Param
primUsedParam i (Param id) = PassOkay $ Param id
primUsedParam i (Default id expr) = primUsedExpr i expr >>= PassOkay . Default id
primUsedParam _ (VarParam) = PassOkay VarParam

primUsedExpr :: Ident -> Expr -> PassResult Expr
primUsedExpr i (Var id) =
  if i == id then
    PassFail ["primUseExpr: detected " ++ show i]
  else
    PassOkay $ Var id
primUsedExpr _ (Const const) = PassOkay $ Const const
primUsedExpr i (Seq exprs) = mapM (primUsedExpr i) exprs >>= PassOkay . Seq
primUsedExpr i (LamAbs params expr) = do
  ps <- mapM (primUsedParam i) params
  e <- primUsedExpr i expr
  return $ LamAbs ps e
primUsedExpr i (LamApp fun args) = do
  as <- mapM (primUsedArg i) args
  f <- primUsedExpr i fun
  return $ LamApp f as
primUsedExpr i (NativeLamApp id ids) = do
  if i == id then
    PassFail ["primUsedExpr: detected " ++ show i]
  else
    PassOkay $ NativeLamApp id ids
primUsedExpr i (Return expr) = primUsedExpr i expr >>= PassOkay . Return
primUsedExpr i (Assign expr1 expr2) = do
  e1 <- primUsedExpr i expr1
  e2 <- primUsedExpr i expr2
  return $ Assign e1 e2
primUsedExpr i (If expr1 expr2 expr3) = do
  e1 <- primUsedExpr i expr1
  e2 <- primUsedExpr i expr2
  e3 <- primUsedExpr i expr3
  return $ If e1 e2 e3
primUsedExpr i (While exprc exprb) = do
  ec <- primUsedExpr i exprc
  eb <- primUsedExpr i exprb
  return $ While ec eb
primUsedExpr i (Break) = PassOkay Break
primUsedExpr i (Next) = PassOkay Next
primUsedExpr i (Error) = PassOkay Error -- Is this fine?
primUsedExpr i (Tix tick expr) = primUsedExpr i expr >>= PassOkay . Tix tick

primUsedPass :: Ident -> Program -> PassResult Program
primUsedPass id (Program exprs) =
  mapM (primUsedExpr id) exprs >>= PassOkay . Program


