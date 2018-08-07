module SimpleR.Interpreter.Preprocessor.Passes.NonPrimAssignPass
  (
  ) where

{-

foldEithersForRight :: [Either a [String]] -> Either [a] [String]
foldEithersForRight eithers =
  foldl (\acc eith -> case acc of
            Left as ->
              case eith of
                Left a -> Left (as ++ [a])
                Right ms -> Right ms
            Right msgs ->
              case eith of
                Left a -> Right msgs
                Right ms -> Right (msgs ++ ms))
    (Left []) eithers

checkParam :: Ident -> Ident -> Param -> Either Param [String]
checkParam _ _ (VarParam) = Left $ VarParam
checkParam o n (Param id) = Left $ Param (rename o n id)
checkParam o n (Default id expr) =
  case checkExpr o n expr of
    Left expr2 -> Left $ Default (rename o n id) expr2
    Right msgs -> Right msgs

renameArg :: Ident -> Ident -> Arg -> Either Arg [String]
renameArg _ _ (VarArg) = Left $ VarArg
renameArg o n (Arg expr) =
  case renameExpr o n expr of
    Left expr2 -> Left $ Arg expr2
    Right msgs -> Right msgs
renameArg o n (Named id expr) =
  case renameExpr o n expr of
    Left expr2 -> Left $ Named (rename o n id) expr2
    Right msgs -> Right msgs

renameExpr :: Ident -> Ident -> Expr -> Either Expr [String]
renameExpr o n expr =
  case expr of
    Var id -> Left $ Var (rename o n id)
    Mem mem -> Left $ Mem mem
    Const const -> Left $ Const const

    Seq exprs ->
      case foldEithersForRight $ map (renameExpr o n) exprs of
        Left exprs2 -> Left $ Seq exprs2
        Right msgs -> Right msgs

    LamAbs params expr ->
      case foldEithersForRight $ map (renameParam o n) params of
        Left params2 ->
          case renameExpr o n expr of
            Left expr2 -> Left $ LamAbs params2 expr2
            Right msgs -> Right msgs
        Right msgs ->
          case renameExpr o n expr of
            Left expr2 -> Right msgs
            Right msgs2 -> Right (msgs ++ msgs2)

    LamApp expr args ->
      case foldEithersForRight $ map (renameArg o n) args of
        Left args2 ->
          case renameExpr o n expr of
            Left expr2 -> Left $ LamApp expr2 (args2)
            Right msg2 -> Right msg2
        Right msgs ->
          case renameExpr o n expr of
            Left expr2 -> Right msgs
            Right msgs2 -> Right (msgs ++ msgs2)

    NativeLamApp id ids ->
      Left $ NativeLamApp (rename o n id) (map (rename o n) ids)
    Return expr ->
      case renameExpr o n expr of
        Left expr2 -> Left $ Return expr2
        Right msgs -> Right msgs

    Assign (Var id) 


      Assign expr1 expr2 -> Assign (rename o n expr1) (rename o n expr2)
      SuperAssign expr1 expr2 ->
        SuperAssign (rename o n expr1) (rename o n expr2)

    If expr1 expr2 expr3 ->
      If (renameExpr o n expr1) (renameExpr o n expr2) (renameExpr o n expr3)
    While expr1 expr2 -> While (renameExpr o n expr1) (renameExpr o n expr2)
    Break -> Break
    Next -> Next
    Error -> Error
    Tix tick expr -> Tix (rename o n tick) (rename o n expr)

-}
