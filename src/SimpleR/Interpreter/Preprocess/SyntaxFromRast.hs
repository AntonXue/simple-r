{-# LANGUAGE MultiParamTypeClasses #-}
module SimpleR.Interpreter.Preprocess.SyntaxFromRast
  ( convert
  ) where

import SimpleR.R.Parser.Syntax
import SimpleR.Language
import SimpleR.Interpreter.Natives

class Convert a b where
  convert :: a -> Int -> (b, Int)

idAnonSeeded :: (String, Int) -> (Ident, Int)
idAnonSeeded (str, int) =
  let name = str ++ "$p1_" ++ show int in
    (Ident { idName = name, idPkg = Nothing, idAnnot = Nothing },
     int + 1)

instance Convert RIdent Ident where
  convert rid int =
    (Ident { idName = ridName rid, idPkg = ridPkg rid, idAnnot = Nothing },
     int)

instance Convert RConst Const where
  convert (RNumConst (RNumInt int)) iint = (IntConst int, iint)
  convert (RNumConst (RNumNaInt)) int = (NaConst, int)
  convert (RNumConst (RNumFloat float)) int = (DoubleConst float, int)
  convert (RNumConst (RNumNaFloat)) int = (NaConst, int)
  convert (RNumConst (RNumComplex complex)) int = (ComplexConst complex, int)
  convert (RNumConst (RNumNaComplex)) int = (NaConst, int)
  convert (RStrConst (RString str)) int = (StringConst str, int)
  convert (RStrConst (RNaString)) int = (NaConst, int)
  convert (RBoolConst (RBool bool)) int = (BoolConst bool, int)
  convert (RBoolConst (RNaBool)) int = (NaConst, int)

instance Convert RParam Param where
  convert (RParam rid) int =
    let (id, int2) = convert rid int in (Param id, int2)
  convert (RDefault rid rexpr) int =
    let (id, int2) = convert rid int in
    let (expr, int3) = convert rexpr int2 in
      (Default id expr, int3)
  convert (RVarParam) int = (VarParam, int)

instance Convert RArg Arg where
  convert (RExprArg rexpr) int =
    let (expr, int2) = convert rexpr int in (Arg expr, int2)
  convert (RIdentAssign rid rexpr) int =
    let (id, int2) = convert rid int in
    let (expr, int3) = convert rexpr int2 in
      (Named id expr, int3)
  convert (RStringAssign (RString str) rexpr) int =
    let id = idFromString str in
    let (expr, int2) = convert rexpr int in
      (Named id expr, int2)
  convert (RVarArg) int = (VarArg, int)
  convert arg _ = error $ "convert: " ++ show arg

instance Convert RUnOp RPrim where
  convert RUMinus int = (RPrimMinus, int)
  convert RUPlus int = (RPrimPlus, int)
  convert RUNot int = (RPrimNot, int)
  convert RUForm int = (RPrimForm, int)
  convert RUHelp int = (RPrimHelp, int)

instance Convert RBinOp RPrim where
  convert RPlus int = (RPrimPlus, int)
  convert RMinus int = (RPrimMinus, int)
  convert RMult int = (RPrimMult, int)
  convert RDiv int = (RPrimDiv, int)
  convert RPow int = (RPrimPow, int)
  convert RMod int = (RPrimMod, int)
  convert RIntDiv int = (RPrimIntDiv, int)
  convert RGt int = (RPrimGt, int)
  convert RGe int = (RPrimGe, int)
  convert RLt int = (RPrimLt, int)
  convert RLe int = (RPrimLe, int)
  convert REq int = (RPrimEq, int)
  convert RNeq int = (RPrimNeq, int)
  convert RAnd int = (RPrimAnd, int)
  convert RAndVec int = (RPrimAnd, int) --
  convert ROr int = (RPrimOr, int)
  convert ROrVec int = (RPrimOr, int) --
  convert RAssign int = (RPrimAssign, int)
  convert RSuperAssign int = (RPrimSuperAssign, int)
  convert RRange int = (RPrimColon, int)
  convert RForm int = (RPrimForm, int)
  convert RHelp int = (RPrimHelp, int)
  -- convert RMatrixMult int
  -- convert ROuterProd int
  -- convert RKronProd int
  -- convert RObjAttr int
  -- convert RGetPackage int
  -- convert RGetPackageInt int
  -- convert RMatch int
  convert binop _ = error $ "convert: unsupported: " ++ show binop

instance Convert RExpr Expr where
  convert (RConst rconst) int =
    let (const, int2) = convert rconst int in (Const const, int2)
  convert (RVar rid) int =
    let (id, int2) = convert rid int in (Var id, int2)
  convert (RNull) int = (Mem memNull, int)
  convert (RUnOp runop rexpr) int =
    let (prim, int2) = convert runop int in
    let (expr, int3) = convert rexpr int2 in
      (LamApp (Var $ idStupidFromRPrim prim) [Arg expr], int3)
  convert (RBinOp rbinop rexpr1 rexpr2) int =
    let (prim, int2) = convert rbinop int in
    let (expr1, int3) = convert rexpr1 int2 in
    let (expr2, int4) = convert rexpr2 int3 in
      (LamApp (Var $ idStupidFromRPrim prim) [Arg expr1, Arg expr2], int4)
  convert (RFunCall rfun rargs) int =
    let (expr, int2) = convert rfun int in
    let (args, int3) = convertList rargs int2 in
      (LamApp expr args, int3)
  convert (RFunDef rparams rexpr) int =
    let (params, int2) = convertList rparams int in
    let (expr, int3) = convert rexpr int2 in
      (LamAbs params expr, int3)
  convert (RSeq rexprs) int =
    let (exprs, int2) = convertList rexprs int in (Seq exprs, int2)
  convert (RIf rexprc rexprt) int =
    let (exprc, int2) = convert rexprc int in
    let (exprt, int3) = convert rexprt int2 in
      (If exprc exprt (Mem memNull), int3)
  convert (RIfElse rexprc rexprt rexprf) int =
    let (exprc, int2) = convert rexprc int in
    let (exprt, int3) = convert rexprt int2 in
    let (exprf, int4) = convert rexprf int3 in
      (If exprc exprt exprf, int4)
  convert (RWhile rexprc rexprb) int =
    let (exprc, int2) = convert rexprc int in
    let (exprb, int3) = convert rexprb int2 in
      (While exprc exprb, int3)


  convert (RRepeat rexpr) int =
    let (expr, int2) = convert rexpr int in
      (While (Const $ BoolConst True) expr, int2)
  convert (RNext) int = (Next, int)
  convert (RBreak) int = (Break, int)
-- convert (RFor rid rexprc rexprb) int =
-- convert (RVecProj rexpr rargs) int =
-- convert (RVecSub rexpr rargs) int =

convertList :: (Convert a b) => [a] -> Int -> ([b], Int)
convertList = undefined
