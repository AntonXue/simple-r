module SimpleR.Interpreter.Preprocessor.Loader
  (
  ) where

import SimpleR.Language
import SimpleR.R
import SimpleR.Interpreter.Natives
import SimpleR.Interpreter.Preprocessor.SyntaxFromRast
import SimpleR.Interpreter.Preprocessor.NamingPass

-- The big picture is that there are two main parsing passes:
--  1. Load the base
--  2. Load the actual program


-- Rename all the prims that appear into their primitive definitions
-- if applicable.

baseDir :: String
baseDir = undefined

baseFile :: String
baseFile = baseDir ++ "/custom.R"

data ExecTree =
    ExprLeaf String Expr
  | ExprNode String [ExecTree]
  deriving (Eq, Show, Read)

{-
unwrapRawString :: String -> String
unwrapRawString str = case str of
  ('"' : chars) ->
    case reverse chars of
      ('"' : trimmed) -> reverse trimmed
      _ -> str
  _ -> str

-}

{-
canonRFile :: String -> String -> String =
canonRFile dir file =
  case file of
    (':' : _) -> file -- absolute path
    _ -> dir ++ "/" ++ file
-}

programFromCanonedRFile :: String -> IO Program
programFromCanonedRFile file = do
  maybeRProg <- parseRFile file
  case maybeRProg of
    (Just rprog) -> return $ fst $ convert rprog 1
    _ -> error $ "programFromCanonedRFile: error parsing " ++ show file


