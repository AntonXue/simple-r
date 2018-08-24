module SimpleR.Interpreter.Commons.SmtTranslation
  (
  ) where

import SimpleR.Language
import SimpleR.Interpreter.Commons.Support
import SimpleR.Interpreter.Commons.SupportUtils
import SimpleR.Interpreter.Commons.Vector
import SimpleR.Smt

smtCanonMem :: MemAddr -> String
smtCanonMem mem = "mem$" ++ (show $ memAddr mem)

smtCanonMemLen :: MemAddr -> String
smtCanonMemLen mem = smtCanonMem mem ++ "$length"

symbFromMem :: MemAddr -> SmtIdent
symbFromMem mem = sidFromString $ smtCanonMem mem

lenSidFromSid :: SmtIdent -> SmtIdent
lenSidFromSid sid = sidFromString $ stringFromSid sid ++ "$length"

symVecFromVec :: MemAddr -> Vector -> State -> (MemAddr, State)
symVecFromVec mem vec state = undefined


