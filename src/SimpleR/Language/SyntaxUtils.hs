module SimpleR.Language.SyntaxUtils where

import SimpleR.Language.Syntax

-- Annotations
defaultIdAnnot :: IdentAnnot
defaultIdAnnot = IdentAnnot

-- Ident
idFromString :: String -> Ident
idFromString str =
  Ident { idName = str, idPkg = Nothing, idAnnot = Nothing }

