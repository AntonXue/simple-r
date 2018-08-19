module SimpleR.R.Parser.Hack where

import Data.List
import Data.List.Utils
import Data.Maybe
import System.IO
import System.Process
import Text.Read

import SimpleR.R.Parser.Syntax

import Debug.Trace

-- listToMaybe :: [a] -> Maybe a
-- listToMaybe [] = Nothing
-- listToMaybe (x:_) = Just x

maybeRead :: (Read a) => String -> Maybe a
maybeRead str = fmap fst $ listToMaybe $ reads str

port_path :: String
port_path =
  "/home/celery/foo/harvard/simple-r/src/SimpleR/R/Parser/hack/absyn_generator.byte"

-- test_str = "RVar (RIdent {rid_pkg = Nothing, rid_name = \"a\", rid_src = Nothing, rid_annot = Nothing})"

-- test_str = "RProgram [RBinOp RAssign (RVar (RIdent {rid_pkg = Nothing, rid_name = \"a\", rid_src = Nothing, rid_annot = Nothing})) (RConst (RNumConst (RNumInt 0)))]"

-- test_str = "RProgram [RFor (RIdent {rid_pkg = Nothing, rid_name = \"i\", rid_src = Nothing, rid_annot = Nothing}) (RBinOp RRange (RConst (RNumConst (RNumInt 1))) (RConst (RNumConst (RNumInt 5)))) (RSeq [RBinOp RAssign (RVar (RIdent {rid_pkg = Nothing, rid_name = \"a\", rid_src = Nothing, rid_annot = Nothing})) (RBinOp RPlus (RVar (RIdent {rid_pkg = Nothing, rid_name = \"a\", rid_src = Nothing, rid_annot = Nothing})) (RVar (RIdent {rid_pkg = Nothing, rid_name = \"i\", rid_src = Nothing, rid_annot = Nothing})))])]"

-- testParse :: Maybe RProgram
-- testParse = maybeRead test_str

uniChars :: [String]
uniChars = (map show [0 .. 9]) ++
          ["A", "B", "C", "D", "E", "F"] ++
          ["a", "b", "c", "d", "e", "f"]

uniReplace :: String -> String
uniReplace str = foldl' (\acc u -> replace ("\\u" ++ u) u acc) str uniChars

parseRFile :: String -> IO (Maybe RProgram)
parseRFile file = do
  traceIO $ "parseRFile: parsing " ++ file
  port_handles <- createProcess (proc port_path [file]) {std_out = CreatePipe}
  case port_handles of
    (_, Just out_handle, _, _) -> do
      rawRawParse <- hGetContents out_handle
      -- let rawParse = uniReplace rawRawParse
      let rawParse = replace "\\u" "u" $ replace "\\u" "u" rawRawParse
      case (maybeRead rawParse) :: Maybe RProgram of
        Nothing -> do
          traceIO $ "parseRFile: " ++ file ++ " parse error"
          traceIO $ ">>>>> start dump"
          traceIO $ "## DUMP OMITTED"
          traceIO $ show rawParse
          traceIO $ "<<<<< end dump"
          return Nothing

        Just rprog -> do
          traceIO $ "parseRFile: " ++ file ++ " success"
          return $ Just rprog

    _ -> do
      traceIO $ "parseRFile: " ++ file ++ " process error"
      return Nothing

