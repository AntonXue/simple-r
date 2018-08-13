module SimpleR.Interpreter.Preprocessor.Passes.FunCalledPass
  ( funCalledPass
  ) where

import SimpleR.Language
import SimpleR.Interpreter.Preprocessor.Passes.PassCommons

-- We want to reject programs that make a function call with a specific
-- identifier.

{-
funUsedExpr :: Ident -> Expr -> PassResult Expr
funUsedExpr _ (Var id) = PassOkay $ Var id
funUsedExpr _ (Const const) = PassOkay $ Const const
funUsedExpr _ (ESq exprs) = mapM 
-}


funCalledPass :: Ident -> Program -> PassResult Program
funCalledPass id (Program exprs) =
  undefined

--   mapM (funUsedExpr id) exprs >>= PassOkay . Program


