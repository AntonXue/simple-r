module SimpleR.Interpreter.Preprocessor.Loader
  (
    linearizeCustomBaseWithPasses
  , linearizeDefaultBaseWithPasses
  , linearizeUserWithPasses
  , linearizeUserWithCustomBaseWithPasses
  , linearizeUserWithDefaultBaseWithPasses
  , loadUserWithNoBase
  , loadUserWithCustomBase
  , loadUserWithDefaultBase
  , loadUserGuessWithNoBase
  , loadUserGuessWithCustomBase
  , loadUserGuessWithDefaultBase
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
linearizeCustomBaseWithPasses ::
  String -> String -> IO (PassResult ([String], [(String, Expr)]))
linearizeCustomBaseWithPasses baseDir baseFile = do
  mbLinearBase <- linearizeCustomBase baseDir baseFile
  case mbLinearBase of
    Just (files, pairs) ->
      case runBasePasses pairs of
        PassOkay okays -> return $ PassOkay (files, okays)
        PassFail msgs -> return $ PassFail msgs
    _ ->
      return $ PassFail ["linearizeCustomBaseWithPasses: failed at " ++
                            baseDir ++ "/" ++ baseFile]

linearizeDefaultBaseWithPasses :: IO (PassResult ([String], [(String, Expr)]))
linearizeDefaultBaseWithPasses = do
  baseDir <- defaultBaseDir
  baseFile <- defaultBaseFile
  linearizeCustomBaseWithPasses baseDir baseFile

linearizeUserWithPasses ::
  String -> String -> IO (PassResult ([String], [(String, Expr)]))
linearizeUserWithPasses dir file = do
  mbLinearUser <- linearizeUser dir file
  case mbLinearUser of
    Just (files, pairs) ->
      case runUserPasses pairs of
        PassOkay okays -> return (PassOkay (files, okays))
        PassFail msgs -> return (PassFail msgs)
    _ ->
      return $ PassFail ["linearizeFileWithPasses: filed to linearize"]

linearizeUserWithCustomBaseWithPasses ::
  String -> String ->
  String -> String ->
    IO (PassResult ([String], [(String, Expr)]))
linearizeUserWithCustomBaseWithPasses baseDir baseFile userDir userFile = do
  basePass <- linearizeCustomBaseWithPasses baseDir baseFile
  userPass <- linearizeUserWithPasses userDir userFile
  case (basePass, userPass) of
    (PassOkay baseLinear, PassOkay userLinear) ->
      return $ PassOkay $ joinLinearizations baseLinear userLinear
    _ -> return $
          PassFail ["linearizeUserWithCustomBase: failed at " ++
                      baseDir ++ "/" ++ baseFile ++ " " ++
                      userDir ++ "/" ++ userFile ]

linearizeUserWithDefaultBaseWithPasses ::
  String -> String -> IO (PassResult ([String], [(String, Expr)]))
linearizeUserWithDefaultBaseWithPasses userDir userFile = do
  baseDir <- defaultBaseDir
  baseFile <- defaultBaseFile
  linearizeUserWithCustomBaseWithPasses baseDir baseFile userDir userFile

--------------------------------------------------------------------
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


-----------
-- Raw initializaiton tuples
rawInitsFromUser ::
  String -> String ->
  (String -> String -> IO (PassResult ([String], [(String, Expr)]))) ->
    IO (Stack, Heap, MemRef, MemRef)
rawInitsFromUser userDir userFile userLinearizer = do
  let heap1 = heapEmpty
  let dummyFile = "$primitives"
  pass <- userLinearizer userDir userFile
  case pass of
    PassOkay ([], []) -> 
      error $ "rawInitsFromFile: failed to initialize"
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
        _ -> error $ "rawInitsFromFile: initialization failed"
    PassFail msgs ->
        error $ "rawInitsFromFile: " ++ (show msgs)

rawInitsFromUserWithNoBase ::
  String -> String -> IO (Stack, Heap, MemRef, MemRef)
rawInitsFromUserWithNoBase userDir userFile = do
  rawInitsFromUser userDir userFile linearizeUserWithPasses

rawInitsFromUserWithDefaultBase ::
  String -> String -> IO (Stack, Heap, MemRef, MemRef)
rawInitsFromUserWithDefaultBase userDir userFile = do
  rawInitsFromUser userDir userFile linearizeUserWithDefaultBaseWithPasses

rawInitsFromUserWithCustomBase ::
  String -> String ->
  String -> String ->
    IO (Stack, Heap, MemRef, MemRef)
rawInitsFromUserWithCustomBase baseDir baseFile userDir userFile =
  rawInitsFromUser userDir userFile
    (linearizeUserWithCustomBaseWithPasses baseDir baseFile)


-- At first, treat the base ids as pure
initPureIds :: [Ident]
initPureIds = primIds

-- Actual state initialization
rawInitState ::
  String -> String ->
  (String -> String -> IO (Stack, Heap, MemRef, MemRef)) ->
    IO State
rawInitState userDir userFile initializer = do
  (stack, heap, globalMem, primMem) <- initializer userDir userFile
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
    _ -> error $ "rawInitState: somehow failed here"

rawInitStateNoBase :: String -> String -> IO State
rawInitStateNoBase userDir userFile =
  rawInitState userDir userFile rawInitsFromUserWithNoBase
  

rawInitStateWithCustomBase :: String -> String -> String -> String -> IO State
rawInitStateWithCustomBase baseDir baseFile userDir userFile =
  rawInitState userDir userFile
    (rawInitsFromUserWithCustomBase baseDir baseFile)

rawInitStateWithDefaultBase :: String -> String -> IO State
rawInitStateWithDefaultBase userDir userFile =
  rawInitState userDir userFile rawInitsFromUserWithDefaultBase

------
-- Putting everything together

guessEntryInfo :: String -> (String, String)
guessEntryInfo userFile =
  let prefix = L.intercalate "/" $ init $ LS.splitOn "/" userFile in
  let suffix = last $ LS.splitOn "/" userFile in
    (prefix, suffix)

loadUserWithNoBase :: String -> String -> IO State
loadUserWithNoBase userDir userFile =
  rawInitState userDir userFile
    (\a b -> rawInitsFromUser a b linearizeUserWithPasses)

loadUserWithCustomBase ::
  String -> String -> String -> String -> IO State
loadUserWithCustomBase baseDir baseFile userDir userFile =
  rawInitStateWithCustomBase baseDir baseFile userDir userFile

loadUserWithDefaultBase ::
  String -> String -> IO State
loadUserWithDefaultBase userDir userFile =
  rawInitStateWithDefaultBase userDir userFile

loadUserGuessWithNoBase :: String -> IO State
loadUserGuessWithNoBase guessFile = do
  let (userDir, userFile) = guessEntryInfo guessFile
  loadUserWithNoBase userDir userFile

loadUserGuessWithCustomBase :: String -> String -> String -> IO State
loadUserGuessWithCustomBase baseDir baseFile guessFile = do
  let (userDir, userFile) = guessEntryInfo guessFile
  loadUserWithCustomBase baseDir baseFile userDir userFile

loadUserGuessWithDefaultBase :: String -> IO State
loadUserGuessWithDefaultBase guessFile = do
  let (userDir, userFile) = guessEntryInfo guessFile
  loadUserWithDefaultBase userDir userFile



