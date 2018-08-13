module SimpleR.Interpreter.Commons.SmtTranslation
  (
  ) where

import SimpleR.Language
import SimpleR.Interpreter.Commons.Support
import SimpleR.Smt

smtIdFromMem :: MemRef -> SmtIdent
smtIdFromMem mem = smtIdFromString $ "mem$" ++ (show $ memAddr mem)

smtIdLenId :: SmtIdent -> SmtIdent
smtIdLenId id = smtIdFromString $ stringFromSmtId id ++ "$length"

smtMemLenId :: MemRef -> SmtIdent
smtMemLenId mem = smtIdLenId $ smtIdFromMem mem

