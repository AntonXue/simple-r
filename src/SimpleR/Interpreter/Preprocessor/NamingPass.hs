module SimpleR.Interpreter.Preprocessor.NamingPass
  (
  ) where

import SimpleR.Language
import SimpleR.R
import SimpleR.Interpreter.Natives

class Renameable a where
  rename :: Ident -> Ident -> a -> a

instance Renameable Ident where
  rename old new id = if old == id then new else id

instance Renameable Tick where
  rename _ _ tick = tick

instance Renameable Param where
  rename _ _ (VarParam) = VarParam
  rename o n (Param id) = Param (rename o n id)
  rename o n (Default id expr) = Default (rename o n id) (rename o n expr)

instance Renameable Arg where
  rename _ _ (VarArg) = VarArg
  rename o n (Arg expr) = Arg (rename o n expr)
  rename o n (Named id expr) = Named (rename o n id) (rename o n expr)

instance Renameable Expr where
  rename o n expr =
    case expr of
      Var id -> Var (rename o n id)
      Const const -> Const const
      Seq exprs -> Seq (map (rename o n) exprs)
      LamAbs params expr -> LamAbs (map (rename o n) params) (rename o n expr)
      LamApp expr args -> LamApp (rename o n expr) (map (rename o n) args)
      NativeLamApp id ids ->
        NativeLamApp (rename o n id) (map (rename o n) ids)
      Return expr -> Return (rename o n expr)
      Assign expr1 expr2 -> Assign (rename o n expr1) (rename o n expr2)
      SuperAssign expr1 expr2 ->
        SuperAssign (rename o n expr1) (rename o n expr2)
      While expr1 expr2 -> While (rename o n expr1) (rename o n expr2)
      Break -> Break
      Next -> Next
      Error -> Error
      Tix tick expr -> Tix (rename o n tick) (rename o n expr)
        



-- Rename all the primitive functions that appear in the base program.
renameBasePrims :: Program -> Program
renameBasePrims (Program exprs) = undefined


