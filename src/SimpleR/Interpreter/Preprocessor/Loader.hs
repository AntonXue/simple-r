module SimpleR.Interpreter.Preprocessor.Loader
  ( baseDir
  , baseFile
  , canonRFile
  , rawProgramFromCanonRFile
  , rawBasePasses
  , rawUserPasses
  , loadFileWithBase
  , loadFileGuessWithBase
  ) where

import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Map as M
import System.Directory

import SimpleR.Language
import SimpleR.R
import SimpleR.Interpreter.Commons
import SimpleR.Interpreter.Natives
import SimpleR.Interpreter.Preprocessor.SyntaxFromRast
import SimpleR.Interpreter.Preprocessor.RunPasses

-- The big picture for the phases:
--  1. Parse the base
--  2. Parse the user program
--  3. Run processing passes on base
--  4. Run processing passes on user program
--  5. Run processing passes on the combination of base + user program


-- Utility stuff
baseDir :: IO String
baseDir = do
  curr <- getCurrentDirectory
  return $ curr ++ "/base/R"

baseFile :: IO String
baseFile = return $ "custom.R"

envMemOffsetMem :: MemRef
envMemOffsetMem = memFromInt 2

data ExecTree =
    ExprLeaf String Expr
  | ExprNode String [ExecTree]
  deriving (Eq, Show, Read)

unwrapRawString :: String -> String
unwrapRawString str = case str of
  ('"' : chars) ->
    case reverse chars of
      ('"' : trimmed) -> reverse trimmed
      _ -> str
  _ -> str

canonRFile :: String -> String -> String
canonRFile dir file =
  case file of
    (':' : _) -> file -- absolute path
    _ -> dir ++ "/" ++ file

-----
-- Parsing the R file + passes

rawProgramFromCanonRFile :: String -> IO Program
rawProgramFromCanonRFile file = do
  maybeRProg <- parseRFile file
  case maybeRProg of
    (Just rprog) -> return $ fst $ convert rprog 1 -- Seed is whatever, here 1
    _ -> error $ "rawProgramFromCanonRFile: error parsing " ++ show file

rawBasePasses :: Program -> Maybe Program
rawBasePasses prog =
  case runBasePasses prog of
    Right msgs -> error $ "rawBasePasses:\n" ++ (L.intercalate "\n" msgs)
    Left passed -> Just passed

rawUserPasses :: Program -> Maybe Program
rawUserPasses prog =
  case runUserPasses prog of
    Right msgs -> error $ "rawBasePasses:\n" ++ (L.intercalate "\n" msgs)
    Left passed -> Just passed

passedBaseFromCanonRFile :: String -> IO Program
passedBaseFromCanonRFile file = do
  baseProg <- rawProgramFromCanonRFile file
  case rawBasePasses baseProg of
    Just passed -> return passed
    _ -> error "passedBaseFromCanonRFile: failed passes on base"

passedUserFromCanonRFile :: String -> IO Program
passedUserFromCanonRFile file = do
  userProg <- rawProgramFromCanonRFile file
  case rawUserPasses userProg of
    Just passed -> return passed
    _ -> error "passedUserFromCanonRFile: failed passes on user program"


-----
-- Set the stuff up so that it's actually ready to load into a State

-- Determine whether an expression is about using `source`, or regular stuff
catFromExpr :: Expr -> Either String Expr
catFromExpr (LamApp (Var (fun @ Ident { idName = "source" }))
                    [Arg (Const (StringConst src))]) = Left src
catFromExpr (LamApp (Var (fun @ Ident { idName = "source" }))
                    [Arg (Var id)]) = Left $ idName id
catFromExpr expr = Right expr

execTreeFromFile :: String -> String -> (String -> IO Program) -> IO ExecTree
execTreeFromFile dir file parser = do
  let canonFile = canonRFile dir file
  Program exprs <- parser canonFile
  let cats = map catFromExpr exprs
  nodes <- mapM (\cat -> case cat of
                    Left childFile -> execTreeFromFile dir childFile parser
                    Right expr -> return $ ExprLeaf canonFile expr)
                cats
  return $ ExprNode canonFile nodes

-----
-- Get the base and user exec trees, which will later be linearized
rawBaseExecTree :: IO ExecTree
rawBaseExecTree = do
  base <- baseDir
  file <- baseFile
  execTreeFromFile base file passedBaseFromCanonRFile

rawUserExecTree :: String -> String -> IO ExecTree
rawUserExecTree dir file = execTreeFromFile dir file passedUserFromCanonRFile

passedBaseExecTree :: IO ExecTree
passedBaseExecTree = rawBaseExecTree -- Nothing yet at this phase

-- Nothing yet at this phase
passedUserExecTree :: String -> String -> IO ExecTree
passedUserExecTree dir file = rawUserExecTree dir file

-----
-- ExecTree processing + linearization

joinExecTree :: ExecTree -> ExecTree -> ExecTree
joinExecTree base user =
  case user of
    ExprLeaf file expr -> ExprNode file $ base : [ExprLeaf file expr]
    ExprNode file childs -> ExprNode file $ base : childs

linearizeExecTree :: ExecTree -> ([String], [(String, Expr)])
linearizeExecTree (ExprLeaf file expr) = ([], [(file, expr)])
linearizeExecTree (ExprNode file childs) =
  let level = map linearizeExecTree childs in
  let (files, exprs) =
          foldl (\(accFs, accEs) (fs, es) -> (accFs ++ fs, accEs ++ es))
                ([], []) level in
    (files ++ [file], exprs)

linearizeFile ::
  String -> String -> (String -> IO Program) -> IO ([String], [(String, Expr)])
linearizeFile dir file parser = do
  execTree <- execTreeFromFile dir file parser
  return $ linearizeExecTree execTree

linearizeFileWithBase ::
  String -> String -> IO ([String], [(String, Expr)])
linearizeFileWithBase dir file = do
  base <- passedBaseExecTree
  user <- passedUserExecTree dir file
  return $ linearizeExecTree $ joinExecTree base user

-- State initialization

-- Allocate a list of environments that correspond to each file,
-- with nesting order such that the first file is the most nested
allocFileEnvs :: [String] -> MemRef -> Heap -> ([(String, MemRef)], Heap)
allocFileEnvs [] _ heap = ([], heap)
allocFileEnvs files botEnvMem heap =
  let envs = map (\f -> DataObj (EnvVal envEmpty) attrsEmpty) files in
  let (envMems, heap2) = heapAllocList envs heap in
  let predMems = init $ botEnvMem : envMems in
  let envMemPredMemPairs = zip envMems predMems in
  let heap3 = foldl (\hp (e, p) ->
            case heapLookup e hp of
              Just (DataObj (EnvVal env) attrs) ->
                let env2 = env { envPredMem = p } in
                  heapInsert e (DataObj (EnvVal env2) attrs) hp
              _ -> error $ "allocFileEnvs: at " ++ show e ++ "\n" ++ show hp)
            heap2 envMemPredMemPairs in
    (zip files envMems, heap3)

-- Takes the [(String, Expr)],
-- which is each file associated with an expression,
-- as well as a [(String, MemRef)], which is the memory for the env of
-- each file, and then flattens them out and stuff
framesFromPairs :: [(String, Expr)] -> [(String, MemRef)] -> [Frame]
framesFromPairs exprPairs filePairs =
  let fileMap = M.fromList filePairs in
    map (\(file, expr) ->
        case M.lookup file fileMap of
          Just mem -> frameMk mem $ EvalSlot expr
          _ -> error $ "framesFromPairs " ++ show file ++ "\n" ++ show fileMap)
        exprPairs

injectPrimBinds :: MemRef -> Heap -> ([(Ident, MemRef)], Heap)
injectPrimBinds primEnvMem heap =
  foldl (\(accs, hp) (id, (params, body)) ->
          let fEnv = envEmpty { envPredMem = primEnvMem } in
          let fEnvObj = DataObj (EnvVal fEnv) attrsEmpty in
          let (fEnvMem, hp2) = heapAlloc fEnvObj hp in
          let funVal = FunVal fEnvMem params body in
          let funObj = DataObj funVal attrsEmpty in
          let (funMem, hp3) = heapAlloc funObj hp2 in
            (accs ++ [(id, funMem)], hp3))
        ([], heap) primInjectionPairs
  
rawInitsFromFileWithBase :: String -> String -> IO (Stack, Heap, MemRef, MemRef)
rawInitsFromFileWithBase dir file = do
  let heap1 = heapEmpty
  (files, fileExprPairs) <- linearizeFileWithBase dir file
  -- Append a dummy file to make environment a space to inject prims
  let dummyFile = "$primitives"
  let files2 = dummyFile : files
  let fileExprPairs2 = (dummyFile, Mem memNull) : fileExprPairs
  -- Allocate the file environments; dummyFile guaranteed to make space
  let (fileEnvPairs, heap2) = allocFileEnvs files2 envMemOffsetMem heap1
  let frames = framesFromPairs fileExprPairs2 fileEnvPairs
  case (fileEnvPairs, reverse fileEnvPairs) of
    ((_, primMem) : _, (_, globalMem) : _) ->
      return (stackPushList frames stackEmpty, heap2, globalMem, primMem)
    _ -> error $ "rawInitsFromFileWithBase: initialization failed"

-- At first, treat the base ids as pure
initPureIds :: [Ident]
initPureIds = primIds

rawInitStateWithBase :: String -> String -> IO State
rawInitStateWithBase dir file = do
  (stack, heap, globalMem, primMem) <- rawInitsFromFileWithBase dir file
  let (primBinds, heap2) = injectPrimBinds primMem heap
  case heapEnvInsertList primBinds primMem heap2 of
    Just heap3 ->
      return $ stateDefault
        { stStack = stack
        , stHeap = heap3
        , stPures = puresInsertList initPureIds puresEmpty
        , stBaseEnvMem = primMem
        , stGlobalEnvMem = globalMem
        }
    _ -> error $ "rawInitStateWithBase: somehow failed here"

guessEntryInfo :: String -> (String, String)
guessEntryInfo file =
  let prefix = L.intercalate "/" $ init $ LS.splitOn "/" file in
  let suffix = last $ LS.splitOn "/" file in
    (prefix, suffix)

loadFileWithBase :: String -> String -> IO State
loadFileWithBase dir file = do
  rawInitStateWithBase dir file

loadFileGuessWithBase :: String -> IO State
loadFileGuessWithBase file = do
  let (dir, tgt) = guessEntryInfo file
  loadFileWithBase dir tgt


