module SimpleR.Interpreter.Preprocess.SyntaxFromRast
  ( convExpr
  ) where

import SimpleR.R.Parser.Syntax
import SimpleR.Language
import SimpleR.Interpreter.Natives

convId :: RIdent -> Ident
convId rid =
  Ident { idName = ridName rid, idPkg = ridPkg rid, idAnnot = IdentAnnot }

convConst :: RConst -> Const
convConst (RNumConst (RNumInt int)) = IntConst int
convConst (RNumConst (RNumNaInt)) = NaConst
convConst (RNumConst (RNumFloat float)) = DoubleConst float
convConst (RNumConst (RNumNaFloat)) = NaConst
convConst (RNumConst (RNumComplex complex)) = ComplexConst complex
convConst (RNumConst (RNumNaComplex)) = NaConst
convConst (RStrConst (RString str)) = StringConst str
convConst (RStrConst (RNaString)) = NaConst
convConst (RBoolConst (RBool bool)) = BoolConst bool
convConst (RBoolConst (RNaBool)) = NaConst

convParam :: RParam -> Param
convParam (RParam rid) = Param $ convId rid
convParam (RDefault rid rexpr) = Default (convId rid) (convExpr rexpr)
convParam (RVarParam) = VarParam

convArg :: RArg -> Arg
convArg (RExprArg rexpr) = Arg (convExpr rexpr)
convArg (RIdentAssign rid rexpr) = Named (convId rid) (convExpr rexpr)
convArg (RStringAssign (RString str) rexpr) =
          Named (idFromString str) (convExpr rexpr)
convArg (RVarArg) = VarArg
convArg arg = error $ "convArg: " ++ show arg

convUnOp :: RUnOp -> RPrim
convUnOp RUMinus = RPrimMinus
convUnOp RUPlus = RPrimPlus
convUnOp RUNot = RPrimNot
convUnOp RUForm = RPrimForm
convUnOp RUHelp = RPrimHelp

convBinOp :: RBinOp -> RPrim
convBinOp RPlus = RPrimPlus
convBinOp RMinus = RPrimMinus
convBinOp RMult = RPrimMult
convBinOp RDiv = RPrimDiv
convBinOp RPow = RPrimPow
convBinOp RMod = RPrimMod
convBinOp RIntDiv = RPrimIntDiv
convBinOp RGt = RPrimGt
convBinOp RGe = RPrimGe
convBinOp RLt = RPrimLt
convBinOp RLe = RPrimLe
convBinOp REq = RPrimEq
convBinOp RNeq = RPrimNeq
convBinOp RAnd = RPrimAnd
convBinOp RAndVec = RPrimAnd --
convBinOp ROr = RPrimOr
convBinOp ROrVec = RPrimOr --
convBinOp RAssign = RPrimAssign
convBinOp RSuperAssign = RPrimSuperAssign
convBinOp RRange = RPrimColon
convBinOp RForm = RPrimForm
convBinOp RHelp = RPrimHelp
-- convBinOp RMatrixMult
-- convBinOp ROuterProd
-- convBinOp RKronProd
-- convBinOp RObjAttr
-- convBinOp RGetPackage
-- convBinOp RGetPackageInt
-- convBinOp RMatch
convBinOp binop = error $ "convBinOp: unsupported: " ++ show binop

convExpr :: RExpr -> Expr
convExpr (RConst rconst) = Const (convConst rconst)
convExpr (RVar rid) = Var (convId rid)
convExpr (RNull) = Mem memNull
convExpr (RUnOp runop rexpr) =
  LamApp (Var $ idStupidFromRPrim $ convUnOp runop)
         [convArg $ RExprArg rexpr]
convExpr (RBinOp rbinop rexpr1 rexpr2) =
  LamApp (Var $ idStupidFromRPrim $ convBinOp rbinop)
         (map (convArg . RExprArg) [rexpr1, rexpr2])
convExpr (RFunCall rfun rargs) =
  LamApp (convExpr rfun)
         (map convArg rargs)
convExpr (RFunDef rparams rexpr) =
  LamAbs (map convParam rparams)
         (convExpr rexpr)
convExpr (RSeq rexprs) =
  Seq (map convExpr rexprs)
convExpr (RIf rexprc rexprt) = convExpr (RIfElse rexprc rexprt RNull)
convExpr (RIfElse rexprc rexprt rexprf) =
  If (convExpr rexprc)
     (convExpr rexprt)
     (convExpr rexprf)
convExpr (RWhile rexprc rexprb) =
  While (convExpr rexprc)
        (convExpr rexprb)

convExpr (RNext) = Next
convExpr (RBreak) = Break
{-
convExpr (RFor rid rexprc rexprb) =
convExpr (RVecProj rexpr rargs) =
convExpr (RVecSub rexpr rargs) =
convExpr (RRepeat rexpr) =
-}



