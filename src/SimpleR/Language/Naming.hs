module SimpleR.Language.Naming
  ( Renameable (..)
  ) where

import SimpleR.Language.Syntax


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
      If expr1 expr2 expr3 ->
        If (rename o n expr1) (rename o n expr2) (rename o n expr3)
      While expr1 expr2 -> While (rename o n expr1) (rename o n expr2)

      VecInd expr args -> VecInd (rename o n expr) (map (rename o n) args)
      VecIndAssign expr1 args expr2 ->
        VecIndAssign (rename o n expr1) (map (rename o n) args)
                     (rename o n expr2)
      VecSub expr args -> VecSub (rename o n expr) (map (rename o n) args)
      VecSubAssign expr1 args expr2 ->
        VecSubAssign (rename o n expr1) (map (rename o n) args)
                     (rename o n expr2)

      ListName expr1 expr2 -> ListName (rename o n expr1) (rename o n expr2)
      ListNameAssign expr1 expr2 expr3 ->
        ListNameAssign (rename o n expr1) (rename o n expr2) (rename o n expr3)

      ObjAttr expr1 expr2 -> ObjAttr (rename o n expr1) (rename o n expr2)
      ObjAttrAssign expr1 expr2 expr3 ->
        ObjAttrAssign (rename o n expr1) (rename o n expr2) (rename o n expr3)

      Break -> Break
      Next -> Next
      Error -> Error
      Tix tick expr -> Tix (rename o n tick) (rename o n expr)

instance Renameable Program where
  rename o n (Program exprs) = Program (map (rename o n) exprs)

