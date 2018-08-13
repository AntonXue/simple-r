module SimpleR.Smt.Parser.SyntaxUtils where

import SimpleR.Smt.Parser.Syntax

stringFromSmtSymb :: SmtSymbol -> String
stringFromSmtSymb (SmtSymbol str) = str

stringFromSmtId :: SmtIdent -> String
stringFromSmtId (SmtIdent symb) = stringFromSmtSymb symb
stringFromSmtId (SmtIdentIndInt symb _) = stringFromSmtSymb symb
stringFromSmtId (SmtIdentIndVar symb) = stringFromSmtSymb symb
stringFromSmtId (SmtIdentQualVar symb _) = stringFromSmtSymb symb
stringFromSmtId (SmtIdentQualSort symb _) = stringFromSmtSymb symb

smtIdFromString :: String -> SmtIdent
smtIdFromString str = SmtIdent $ SmtSymbol str

