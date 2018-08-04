module SimpleR.Interpreter.Preprocess.SyntaxFromRast
  ( convExpr
  ) where

import SimpleR.R.Parser.Syntax
import SimpleR.Language

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
convArg (RStringAssign (RString str) rexpr) = Named (mkId str) (convExpr rexpr)
convArg (RVarArg) = VarArg
convArg arg = error $ "convArg: " ++ show arg

convUnOp :: RUnOp -> Ident
convUnOp = undefined

convBinOp :: RBinOp -> Ident
convBinOp = undefined

convExpr :: RExpr -> Expr
convExpr = undefined



