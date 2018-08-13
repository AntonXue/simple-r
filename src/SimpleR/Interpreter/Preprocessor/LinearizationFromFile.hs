{-# LANGUAGE MultiParamTypeClasses #-}
module SimpleR.Interpreter.Preprocessor.LinearizationFromFile
  ( baseDir
  , baseFile
  , canonRFile
  , progFromFile
  , linearizeBase
  , linearizeFile
  , joinLinearizations
  , linearizeFileWithBase
  ) where

import System.Directory

import SimpleR.R
import SimpleR.Language
import SimpleR.Interpreter.Commons
import SimpleR.Interpreter.Natives

-- Conversion stuff
class Convertable a b where
  convert :: a -> Int -> (b, Int)

idAnonSeeded :: String -> Int -> (Ident, Int)
idAnonSeeded str int =
  let name = str ++ "$p1_" ++ show int in
    (Ident { idName = name, idPkg = Nothing, idAnnot = Nothing },
     int + 1)

instance Convertable RIdent Ident where
  convert rid int =
    (Ident { idName = ridName rid, idPkg = ridPkg rid, idAnnot = Nothing },
     int)

instance Convertable RConst Const where
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
  convert (RNaConst) int = (NaConst, int)

instance Convertable RParam Param where
  convert (RParam rid) int =
    let (id, int2) = convert rid int in (Param id, int2)
  convert (RDefault rid rexpr) int =
    let (id, int2) = convert rid int in
    let (expr, int3) = convert rexpr int2 in
      (Default id expr, int3)
  convert (RVarParam) int = (VarParam, int)

instance Convertable RArg Arg where
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

instance Convertable RUnOp RPrim where
  convert RUMinus int = (RPrimMinus, int)
  convert RUPlus int = (RPrimPlus, int)
  convert RUNot int = (RPrimNot, int)
  convert RUForm int = (RPrimForm, int)
  convert RUHelp int = (RPrimHelp, int)

instance Convertable RBinOp RPrim where
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
  convert RGetPackage int = (RPrimGetPackage, int)
  convert RGetPackageInt int = (RPrimGetPackageInt, int)
  -- convert RMatch int
  convert binop _ = error $ "convert: unsupported: " ++ show binop

instance Convertable RExpr Expr where
  convert (RConst rconst) int =
    let (const, int2) = convert rconst int in (Const const, int2)
  convert (RVar rid) int =
    let (id, int2) = convert rid int in (Var id, int2)
  convert (RNull) int = (Var idNull, int)
  convert (RUnOp runop rexpr) int =
    let (prim, int2) = convert runop int in
    let (expr, int3) = convert rexpr int2 in
      (lamAppPrim prim [Arg expr], int3)

  -- Assignment
  convert (RBinOp RAssign rexpr1 rexpr2) int =
    let (expr1, int2) = convert rexpr1 int in
    let (expr2, int3) = convert rexpr2 int2 in
      (Assign expr1 expr2, int3)

  -- Super assignment
  -- convert (RBinOp RSuperAssign rexpr1 rexpr2) int =
  --   let (expr1, int2) = convert rexpr1 int in
  --   let (expr2, int3) = convert rexpr2 int2 in
  --     (SuperAssign expr1 expr2, int3)

  -- Colon operator
  convert (RBinOp RRange rexpr1 rexpr2) int =
    let (expr1, int2) = convert rexpr1 int in
    let (expr2, int3) = convert rexpr2 int2 in
      (lamAppPrim RPrimColon [Arg expr1, Arg expr2], int3)

  convert (RBinOp rbinop rexpr1 rexpr2) int =
    let (prim, int2) = convert rbinop int in
    let (expr1, int3) = convert rexpr1 int2 in
    let (expr2, int4) = convert rexpr2 int3 in
      (lamAppPrim prim [Arg expr1, Arg expr2], int4)
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
      (If exprc exprt (Var idNull), int3)
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
  convert (RVecProj rexpr rargs) int =
    let (expr, int2) = convert rexpr int in
    let (args, int3) = convertList rargs int2 in
      (lamAppPrim RPrimVecProj (Arg expr : args), int3)
  convert (RVecSub rexpr rargs) int =
    let (expr, int2) = convert rexpr int in
    let (args, int3) = convertList rargs int2 in
      (lamAppPrim RPrimVecSub (Arg expr : args), int3)
  convert (RFor rid rexprc rexprb) int =
    let (vecId, int2) = idAnonSeeded "vec" int in
    let (iterId, int3) = idAnonSeeded "iter" int2 in
    let (highId, int4) = idAnonSeeded "high" int3 in
    let (elemId, int5) = convert rid int4 in
    let (exprc, int6) = convert rexprc int5 in
    let (exprb, int7) = convert rexprb int6 in
      (Seq
        [ Assign (Var vecId) exprc
        , Assign (Var iterId) (Const $ IntConst 1)
        -- MAY have problems with just calling length this way.
        , Assign (Var highId) (lamAppPrim RPrimLength [Arg $ Var vecId])
        , While (lamAppPrim RPrimLe [Arg $ Var iterId, Arg $ Var highId])
            (Seq [ Assign (Var elemId)
                          (lamAppPrim RPrimVecProj
                                      [Arg $ Var vecId, Arg $ Var iterId])
                 , exprb
                 , Assign (Var iterId)
                          (lamAppPrim RPrimPlus
                              [Arg $ Var iterId, Arg $ Const $ IntConst 1])

            ])
        ], int7)

lamAppPrim :: RPrim -> [Arg] -> Expr
lamAppPrim prim args =
  -- LamApp (Var $ idStupidFromRPrim prim) args
  LamApp (Var $ idFromRPrim prim) args

convertList :: (Convertable a b) => [a] -> Int -> ([b], Int)
convertList as int =
  foldl (\(acc, i) a -> let (b, i2) = convert a i in (acc ++ [b], i2))
        ([], int) as

instance Convertable RProgram Program where
  convert (RProgram rexprs) int =
    let (exprs, int2) = convertList rexprs int in
      (Program exprs, int2)

-- Load the program
baseDir :: IO String
baseDir = do
  curr <- getCurrentDirectory
  return $ curr ++ "/base/R"

baseFile :: IO String
baseFile = return $ "custom.R"

canonRFile :: String -> String -> String
canonRFile dir file =
  case file of
    (':' : _) -> file -- absolute path
    _ -> dir ++ "/" ++ file

progFromFile :: String -> IO Program
progFromFile file = do
  maybeRProg <- parseRFile file
  case maybeRProg of
    Just rprog -> return $ fst $ convert rprog 1
    _ -> error $ "progFromFile: failed to parse " ++ show file

-- Execution tree linearization

data ExecTree =
    ExprLeaf String Expr
  | ExprNode String [ExecTree]
  deriving (Eq, Show, Read)

catFromExpr :: Expr -> Either String Expr
catFromExpr (LamApp (Var (fun @ Ident { idName = "source" }))
                    [Arg (Const (StringConst src))]) = Left src
catFromExpr (LamApp (Var (fun @ Ident { idName = "source" }))
                    [Arg (Var id)]) = Left $ idName id
catFromExpr expr = Right expr

execTreeFromFile :: String -> String -> IO ExecTree
execTreeFromFile dir file = do
  let canonFile = canonRFile dir file
  Program exprs <- progFromFile canonFile
  let cats = map catFromExpr exprs
  nodes <- mapM (\cat -> case cat of
                    Left childFile -> execTreeFromFile dir childFile
                    Right expr -> return $ ExprLeaf canonFile expr)
                cats
  return $ ExprNode canonFile nodes

baseExecTree :: IO ExecTree
baseExecTree = do
  base <- baseDir
  file <- baseFile
  execTreeFromFile base file 

fileExecTree :: String -> String -> IO ExecTree
fileExecTree dir file = execTreeFromFile dir file

linearizeExecTree :: ExecTree -> ([String], [(String, Expr)])
linearizeExecTree (ExprLeaf file expr) = ([], [(file, expr)])
linearizeExecTree (ExprNode file childs) =
  let level = map linearizeExecTree childs in
  let (files, exprs) =
          foldl (\(accFs, accEs) (fs, es) -> (accFs ++ fs, accEs ++ es))
                ([], []) level in
    (files ++ [file], exprs)

linearizeBase :: IO ([String], [(String, Expr)])
linearizeBase = baseExecTree >>= return . linearizeExecTree

linearizeFile :: String -> String -> IO ([String], [(String, Expr)])
linearizeFile dir file = fileExecTree dir file >>= return . linearizeExecTree

joinLinearizations ::
  ([String], [(String, Expr)]) -> ([String], [(String, Expr)]) ->
    ([String], [(String, Expr)])
joinLinearizations (files1, pairs1) (files2, pairs2) =
  (files1 ++ files2, pairs1 ++ pairs2)

linearizeFileWithBase :: String -> String -> IO ([String], [(String, Expr)])
linearizeFileWithBase dir file = do
  baseLinear <- linearizeBase
  fileLinear <- linearizeFile dir file
  return $ joinLinearizations baseLinear fileLinear


