module SimpleR.Interpreter.Preprocessor.Loader
  (
    linearizeBaseWithPasses
  , linearizeFileWithPasses
  , linearizeFileWithBaseWithPasses
  , loadFileWithBase
  , loadFileGuessWithBase
  ) where

import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Map as M
import System.Directory

import Debug.Trace

import SimpleR.Language
import SimpleR.R
import SimpleR.Interpreter.Commons
import SimpleR.Interpreter.Natives
import SimpleR.Interpreter.Preprocessor.LinearizationFromFile
import SimpleR.Interpreter.Preprocessor.Passes

-- The big picture for the phases:
--  1. Parse the base
--  2. Parse the user program
--  3. Run processing passes on base
--  4. Run processing passes on user program
--  5. Run processing passes on the combination of base + user program

-- Responsible 

-----
-- Parsing the R file + passes
linearizeBaseWithPasses :: IO (PassResult ([String], [(String, Expr)]))
linearizeBaseWithPasses = do
  (files, pairs) <- linearizeBase
  case runBasePasses pairs of
    PassOkay okays -> return (PassOkay (files, okays))
    PassFail msgs -> return (PassFail msgs)

linearizeFileWithPasses ::
  String -> String -> IO (PassResult ([String], [(String, Expr)]))
linearizeFileWithPasses dir file = do
  (files, pairs) <- linearizeFile dir file
  case runUserPasses pairs of
    PassOkay okays -> return (PassOkay (files, okays))
    PassFail msgs -> return (PassFail msgs)

linearizeFileWithBaseWithPasses ::
  String -> String -> IO (PassResult ([String], [(String, Expr)]))
linearizeFileWithBaseWithPasses dir file = do
  basePass <- linearizeBaseWithPasses
  filePass <- linearizeFileWithPasses dir file
  case (basePass, filePass) of
    (PassOkay baseLinear, PassOkay fileLinear) ->
      return $ PassOkay $ joinLinearizations baseLinear fileLinear

-----------------
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
          Just mem -> frameMk mem $ ExprCont expr
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
  
-- Environment settings
envMemOffsetMem :: MemRef
envMemOffsetMem = memFromInt 2

-- Extracting the bare minimum
rawInitsFromFileWithBase :: String -> String -> IO (Stack, Heap, MemRef, MemRef)
rawInitsFromFileWithBase dir file = do
  let heap1 = heapEmpty
  let dummyFile = "$primitives"
  pass <- linearizeFileWithBaseWithPasses dir file
  case pass of
    PassOkay (files, fileExprPairs) -> do
      -- Append a dummy file to make environment a space to inject prims
      let files2 = dummyFile : files
      let fileExprPairs2 = (dummyFile, Var idNull) : fileExprPairs
      -- Allocate the file environments; dummyFile guaranteed to make space
      let (fileEnvPairs, heap2) = allocFileEnvs files2 envMemOffsetMem heap1
      let frames = framesFromPairs fileExprPairs2 fileEnvPairs
      case (fileEnvPairs, reverse fileEnvPairs) of
        ((_, primMem) : _, (_, globalMem) : _) ->
          return (stackPushList frames stackEmpty, heap2, globalMem, primMem)
        _ -> error $ "rawInitsFromFileWithBase: initialization failed"
    PassFail msgs ->
        error $ "rawInitsFromFileWithBase: " ++ (show msgs)

-- At first, treat the base ids as pure
initPureIds :: [Ident]
initPureIds = primIds

-- Actual state initialization
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

------
-- Putting everything together

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




