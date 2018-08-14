module SimpleR.Smt.Parser.SyntaxUtils where

import SimpleR.Smt.Parser.Syntax

stringFromSymb :: SmtSymbol -> String
stringFromSymb (SmtSymbol str) = str

stringFromSid :: SmtIdent -> String
stringFromSid (SmtIdent symb) = stringFromSymb symb
stringFromSid (SmtIdentIndInt symb _) = stringFromSymb symb
stringFromSid (SmtIdentIndVar symb) = stringFromSymb symb
stringFromSid (SmtIdentQualVar symb _) = stringFromSymb symb
stringFromSid (SmtIdentQualSort symb _) = stringFromSymb symb

sidFromString :: String -> SmtIdent
sidFromString str = SmtIdent $ SmtSymbol str

