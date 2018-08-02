module SimpleR.Language.SyntaxUtils where

import SimpleR.Language.Syntax

-- Annotations
defaultIdAnnot :: IdentAnnot
defaultIdAnnot = IdentAnnot

-- MemRef
memFromInt :: Int -> MemRef
memFromInt int = MemRef {smem_addr = int}

memNull :: MemRef
memNull = memFromInt 0

memNext :: MemRef -> MemRef
memNext mem = memFromInt $ 1 + smem_addr mem

-- Ident
idDefault :: Ident
idDefault = idFromString ""

idFromString :: String -> Ident
idFromString str =
  Ident {sid_name = str, sid_pkg = Nothing, sid_annot = defaultIdAnnot}

