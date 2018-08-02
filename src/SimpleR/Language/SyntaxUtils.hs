module SimpleR.Language.SyntaxUtils where

import SimpleR.Language.Syntax

-- Annotations
defaultIdAnnot :: IdentAnnot
defaultIdAnnot = IdentAnnot

-- MemRef
memFromInt :: Int -> MemRef
memFromInt int = MemRef {mem_addr = int}

memNull :: MemRef
memNull = memFromInt 0

memNext :: MemRef -> MemRef
memNext mem = memFromInt $ 1 + mem_addr mem

-- Ident
idDefault :: Ident
idDefault = idFromString ""

idFromString :: String -> Ident
idFromString str =
  Ident {id_name = str, id_pkg = Nothing, id_annot = defaultIdAnnot}

