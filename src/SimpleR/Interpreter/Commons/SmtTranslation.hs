module SimpleR.Interpreter.Commons.SmtTranslation
  (
  ) where

import SimpleR.Language
import SimpleR.Interpreter.Commons.Support
import SimpleR.Interpreter.Commons.SupportUtils
import SimpleR.Interpreter.Commons.Vector
import SimpleR.Smt

smtCanonMem :: MemRef -> String
smtCanonMem mem = "mem$" ++ (show $ memAddr mem)

smtCanonMemLen :: MemRef -> String
smtCanonMemLen mem = smtCanonMem mem ++ "$length"

symbFromMem :: MemRef -> SmtIdent
symbFromMem mem = sidFromString $ smtCanonMem mem

lenSidFromSid :: SmtIdent -> SmtIdent
lenSidFromSid sid = sidFromString $ stringFromSid sid ++ "$length"

symVecFromVec :: MemRef -> Vector -> State -> (MemRef, State)
symVecFromVec mem vec state = undefined


