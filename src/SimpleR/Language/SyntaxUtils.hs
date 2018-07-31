{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-} 
module SimpleR.Language.SyntaxUtils where

import Data.Complex as C

import SimpleR.Language.Syntax

-- Annotations
defaultIdAnnot :: SIdentAnnot
defaultIdAnnot = SIdentAnnot

-- MemRef
memFromInt :: Int -> SMemRef
memFromInt int = SMemRef {smem_addr = int}

memNull :: SMemRef
memNull = memFromInt 0

memNext :: SMemRef -> SMemRef
memNext mem = memFromInt $ 1 + smem_addr mem

-- Ident
idDefault :: SIdent
idDefault = idFromString ""

idFromString :: String -> SIdent
idFromString str =
  SIdent {sid_name = str, sid_pkg = Nothing, sid_annot = defaultIdAnnot}

