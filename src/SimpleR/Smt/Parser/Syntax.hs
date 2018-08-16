module SimpleR.Smt.Parser.Syntax where

data SmtLogic =
    SmtLogAll
  | SmtLogQFUF
  | SmtLogQFLIA
  | SmtLogQFLRA
  | SmtLogQFNIA
  | SmtLogQFNRA
  | SmtLogQFLIRA
  | SmtLogQFNIRA
  deriving (Eq, Show, Read)

data SmtSymbol = SmtSymbol String deriving (Eq, Show, Read)

data SmtIdent =
    SmtIdent SmtSymbol
  | SmtIdentIndInt SmtSymbol Int
  | SmtIdentIndVar SmtSymbol
  | SmtIdentQualVar SmtSymbol SmtSymbol
  | SmtIdentQualSort SmtSymbol SmtSort
  deriving (Eq, Show, Read)

data SmtSort =
    SmtSortInt
  | SmtSortDouble
  | SmtSortBool
  | SmtSortBitVec Int
  | SmtSortArray [SmtSort] SmtSort
  | SmtSortApp SmtIdent [SmtSort]
  deriving (Eq, Show, Read)

data SmtConst = SmtConst String deriving (Eq, Show, Read)

data SmtKeyword = SmtKeyword String deriving (Eq, Show, Read)

data SmtCompExpr =
    SmtGt SmtExpr SmtExpr
  | SmtGe SmtExpr SmtExpr
  | SmtLt SmtExpr SmtExpr
  | SmtLe SmtExpr SmtExpr
  | SmtEq SmtExpr SmtExpr
  | SmtNeq SmtExpr SmtExpr
  deriving (Eq, Show, Read)

data SmtLogicExpr =
    SmtNot SmtExpr
  | SmtAnd SmtExpr SmtExpr
  | SmtOr SmtExpr SmtExpr
  deriving (Eq, Show, Read)

data SmtMathExpr =
    SmtPlus SmtExpr SmtExpr
  | SmtMinus SmtExpr SmtExpr
  | SmtMult SmtExpr SmtExpr
  | SmtDiv SmtExpr SmtExpr
  | SmtExp SmtExpr SmtExpr
  | SmtMod SmtExpr SmtExpr
  | SmtRem SmtExpr SmtExpr
  deriving (Eq, Show, Read)

data SmtArrayExpr =
    SmtArrayGet SmtExpr SmtExpr
  | SmtArraySet SmtExpr SmtExpr SmtExpr
  deriving (Eq, Show, Read)

data SmtExpr =
    SmtVar SmtIdent
  | SmtComp SmtCompExpr
  | SmtLogic SmtLogicExpr
  | SmtMath SmtMathExpr
  | SmtArray SmtArrayExpr
  | SmtApp SmtIdent [SmtExpr]
  | SmtLet [(SmtIdent, SmtExpr)] SmtExpr
  | SmtForAll [(SmtSymbol, SmtSort)] SmtExpr
  | SmtExists [(SmtSymbol, SmtSort)] SmtExpr
  deriving (Eq, Show, Read)

data SmtCmd =
    SmtSetLogic SmtLogic

  | SmtDeclFun SmtSymbol [SmtSort] SmtSort
  | SmtDefFun SmtSymbol [(SmtSymbol, SmtSort)] SmtSort SmtExpr
  | SmtDeclSort SmtSymbol Int
  | SmtDefSort SmtSymbol [SmtSymbol] SmtExpr

  | SmtAssert SmtExpr
  | SmtGetAssertions

  | SmtCheckSat
  | SmtGetModel
  | SmtGetProof
  | SmtGetUnsatCore

  | SmtGetValue [SmtExpr]
  | SmtGetAssignment

  | SmtPush Int
  | SmtPop Int

  | SmtGetOption SmtKeyword
  | SmtSetOption SmtKeyword SmtConst

  | SmtGetInfo SmtKeyword
  | SmtSetInfo SmtKeyword SmtConst

  | SmtExit

  | SmtSat
  | SmtUnsat
  | SmtModel [SmtCmd]
  deriving (Eq, Show, Read)

data SmtProg = SmtProg [SmtCmd] deriving (Eq, Show, Read)


