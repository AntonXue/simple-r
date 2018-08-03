module SimpleR.Language.SyntaxUtils where

import SimpleR.Language.Syntax

-- Annotations
defaultIdAnnot :: IdentAnnot
defaultIdAnnot = IdentAnnot

-- MemRef
mkMem :: Int -> MemRef
mkMem int = MemRef { memAddr = int }

memNull :: MemRef
memNull = mkMem 0

memNext :: MemRef -> MemRef
memNext mem = mkMem $ 1 + memAddr mem

-- Ident
idDefault :: Ident
idDefault = mkId ""

mkId :: String -> Ident
mkId str = Ident { idName = str, idPkg = Nothing, idAnnot = defaultIdAnnot }

