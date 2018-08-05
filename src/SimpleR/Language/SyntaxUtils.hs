module SimpleR.Language.SyntaxUtils where

import SimpleR.Language.Syntax

-- Annotations
defaultIdAnnot :: IdentAnnot
defaultIdAnnot = IdentAnnot

-- MemRef
memFromInt :: Int -> MemRef
memFromInt int = MemRef { memAddr = int }

memNull :: MemRef
memNull = memFromInt 0

memNext :: MemRef -> MemRef
memNext mem = memFromInt $ 1 + memAddr mem

-- Ident
idDefault :: Ident
idDefault = idFromString ""

idFromString :: String -> Ident
idFromString str =
  Ident { idName = str, idPkg = Nothing, idAnnot = Nothing }

