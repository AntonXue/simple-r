module SimpleR.Interpreter.Preprocessor.Passes.PureArgsPass
  ( pureArgsPass
  ) where

import qualified Data.Set as S

import SimpleR.Language
import SimpleR.Interpreter.Natives
import SimpleR.Interpreter.Preprocessor.Passes.PassCommons

primPureSet :: S.Set Ident
primPureSet = S.fromList primIds

data Ok = Ok deriving (Ord, Eq, Show, Read)


checkArg :: Arg -> PassResult Arg
checkArg (Arg expr) = argExpr expr >>= PassOkay . Arg
checkArg (Named id expr) = argExpr expr >>= PassOkay . Named id
checkArg (VarArg) = PassOkay VarArg


-- The expressions that occur within an expression
argExpr :: Expr -> PassResult Expr
-- Should be allowed to refer to variables
argExpr (Var id) = PassOkay $ Var id

-- Should be allowed to refer to memories since we do not use PromiseObj
-- argExpr (Mem mem) = PassOkay $ Mem mem

-- Constants are pure
argExpr (Const const) = PassOkay $ Const const

-- Only if everything here is pure
argExpr (Seq exprs) = mapM argExpr exprs >>= PassOkay . Seq

-- Function abstraction is pure if body is pure,
-- but why would you want this in an Arg???
argExpr (LamAbs params expr) = argExpr expr >>= PassOkay . LamAbs params

-- Function application stuff can only happen without abstractions here
argExpr (LamApp (Var fun) args) =
  if fun `elem` primPureSet then
    mapM checkArg args >>= PassOkay . LamApp (Var fun)
  else
    PassFail ["argExpr: " ++ show fun ++ " call in argument is not in prim"]

argExpr (LamApp _ _) =
  PassFail ["argExpr: arbitrary function application in argument"]
argExpr (NativeLamApp id ids) = PassOkay $ NativeLamApp id ids

-- Controls like Return, Break, Next, are disallowed
argExpr (Return expr) = PassFail ["argExpr: 'return' in argument"]
argExpr (Break) = PassFail ["argExpr: 'break' in argument"]
argExpr (Next) = PassFail ["argExpr: 'next' in argument"]
argExpr (Error) = PassFail ["argExpr: 'ERROR' in argument"]

-- Assignments should not be allowed
argExpr (Assign _ _) = PassFail ["argExpr: assignment in argument"]

-- While loops in arguments? I guess??
argExpr (While exprc exprb) = do
  ec <- argExpr exprc
  eb <- argExpr exprb
  return $ While ec eb
  
-- If statements? Sure?
argExpr (If exprc exprt exprf) = do
  ec <- argExpr exprc
  et <- argExpr exprt
  ef <- argExpr exprf
  return $ If ec et ef
 
-- Tix are just annotations, so make sure the expression is still pure
argExpr (Tix tick expr) = argExpr expr >>= PassOkay . Tix tick



-- The expressions that appear in regular program body

progExpr :: Expr -> PassResult Expr
progExpr (Var id) = PassOkay $ Var id
-- progExpr (Mem mem) = PassOkay $ Mem mem
progExpr (Const const) = PassOkay $ Const const
progExpr (Seq exprs) = mapM progExpr exprs >>= PassOkay . Seq
progExpr (LamAbs params expr) = progExpr expr >>= PassOkay . LamAbs params
progExpr (LamApp fun args) = do
  as <- mapM checkArg args
  f <- progExpr fun
  return $ LamApp f as
progExpr (NativeLamApp id ids) = PassOkay $ NativeLamApp id ids
progExpr (Return expr) = progExpr expr >>= PassOkay . Return
progExpr (Assign expr1 expr2) = do
  e1 <- progExpr expr1
  e2 <- progExpr expr2
  return $ Assign e1 e2
progExpr (If expr1 expr2 expr3) = do
  e1 <- progExpr expr1
  e2 <- progExpr expr2
  e3 <- progExpr expr3
  return $ If e1 e2 e3
progExpr (While exprc exprb) = do
  ec <- progExpr exprc
  eb <- progExpr exprb
  return $ While ec eb
progExpr (Break) = PassOkay Break
progExpr (Next) = PassOkay Next
progExpr (Error) = PassOkay Error -- Is this fine?
progExpr (Tix tick expr) = progExpr expr >>= PassOkay . Tix tick


pureArgsPass :: Program -> PassResult Program
pureArgsPass (Program exprs) = mapM progExpr exprs >>= PassOkay . Program


