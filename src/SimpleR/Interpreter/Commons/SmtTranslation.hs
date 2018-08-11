module SimpleR.Interpreter.Commons.SmtTranslation
  (
  ) where

import SimpleR.Language
import SimpleR.Interpreter.Commons.Support
import SimpleR.Smt

smtIdentFromMem :: MemRef -> SmtIdent
smtIdentFromMem mem = SmtIdent $ SmtSymbol $ "mem$" ++ (show $ memAddr mem)

