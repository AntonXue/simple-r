module SimpleR.Interpreter.Preprocessor.TestPasses
  ( testPassesOnDir
  ) where

import System.Directory
import Data.Char
import Data.List
import Data.Maybe

import SimpleR.Language
import SimpleR.Interpreter.Natives
import SimpleR.Interpreter.Preprocessor.Passes
import SimpleR.Interpreter.Preprocessor.LinearizationFromFile
import SimpleR.Interpreter.Preprocessor.Loader

testPureArgCallFails :: Program -> Bool
testPureArgCallFails prog =
  case pureArgsPass prog of
    PassOkay _ -> False
    PassFail _ -> True

testRmCallFails :: Program -> Bool
testRmCallFails prog =
  case funCalledPass (idFromString "rm") prog of
    PassOkay _ -> False
    PassFail _ -> True

testObjAttrFails :: Program -> Bool
testObjAttrFails prog =
  case primUsedPass (idPrimFromString "@") prog of
    PassOkay _ -> False
    PassFail _ -> True

testIdUsed :: Ident -> Program -> Bool
testIdUsed id prog =
  rename id (idFromString "FUFUFUFUFUFUF") prog /= prog

flaggedPrims :: [RPrim]
flaggedPrims =
  [RPrimObjAttr, RPrimForm, RPrimHelp, RPrimDim, RPrimSuperAssign,
   RPrimGetPackage, RPrimGetPackageInt,
   RPrimDim, RPrimDimAssign,
   RPrimDimNames, RPrimDimNamesAssign,
   RPrimLength, RPrimLengthAssign,
   RPrimLevels, RPrimLevelsAssign,
   RPrimNames, RPrimNamesAssign,
   RPrimGamma, RPrimLGamma, RPrimDiGamma, RPrimTriGamma]

testPrimsUsed :: Program -> [RPrim]
testPrimsUsed prog =
  filter ((flip testIdUsed) prog . idFromRPrim) flaggedPrims

flaggedIds :: [Ident]
flaggedIds =
  (map idFromString ["matrix", "class"])
  ++
  map idFromRPrim flaggedPrims

testFlaggedIdsUsed :: Program -> [Ident]
testFlaggedIdsUsed prog =
  filter ((flip testIdUsed) prog) flaggedIds

flaggedStrings :: [String]
flaggedStrings = ["ObjAttr ", "ObjAttrAssign ",
                  "VecInd ", "VecIndAssign ",
                  "VecSub ", "VecSubAssign ",
                  "ListName ", "ListNameAssign ",
                  "RPrimSuperAssign",
                  "LamApp (Var (Ident {idName = \"class\"",
                  "LamApp (Var (Ident {idName = \"new.env\"",
                  "LamApp (Var (Ident {idName = \"rm\"",
                  "LamApp (Var (Ident {idName = \"setClass\"",
                  "LamApp (Var (Ident {idName = \"setRefClass\"",
                  "LamApp (Var (Ident {idName = \"eval\"",
                  "LamApp (Var (Ident {idName = \"library\"",
                  "LamApp (Var (Ident {idName = \"list2env\""
                  ]

dropFirstLast :: [a] -> [a]
dropFirstLast xs = reverse $ drop 1 $ reverse $ drop 1 xs

bar :: String
bar = "********************************************************************"

testPassesOnDir :: String -> IO ()
testPassesOnDir dir = do
  -- Figure out which things to parse
  files <- getDirectoryContents dir
  let onlyRFiles = sortBy (\a b -> (map toUpper a) `compare` (map toUpper b)) $
                   filter (\f -> (not (".swp" `isSuffixOf` f)) &&
                                 (not (f == ".")) &&
                                 (not (f == ".."))) files
                   -- filter (isSuffixOf ".R") files

  -- Load the Program(s)
  allMbProgs <- mapM progFromFile $ map (canonRFile dir) onlyRFiles
  let allPairs = zip onlyRFiles allMbProgs
  let numAllPairs = length allPairs
  let parsedPairs = map (\(f, mbP) -> (f, fromJust mbP)) $
                        filter (\(_, mbProg) -> mbProg /= Nothing) allPairs
  let numParsedPairs = length parsedPairs

  -- Printing this makes IO force everything before to finish
  putStrLn ""
  putStrLn bar
  putStrLn "PARSING TEST"
  putStrLn "files:"
  _ <- mapM (\(f, _) -> putStrLn $ "  " ++ f) parsedPairs
  putStrLn $ "total: " ++ show numParsedPairs ++ "/" ++ show numAllPairs

  -- Test: flagged strings
  putStrLn ""
  putStrLn bar
  putStrLn "STRING EXISTENCE TEST"

  let strFilesPairs =
        map (\s ->
              (s, concatMap
                    (\(f, p) -> if s `isInfixOf` (show p) then [f] else [])
                    parsedPairs))
            flaggedStrings

  _ <- mapM (\(s, fs) -> do
              putStrLn ""
              putStrLn $ "  " ++ s
              _ <- mapM (\f -> putStrLn $ "    " ++ f) fs
              putStrLn "--")
            strFilesPairs

  putStrLn $ "flagged string counts:"
  _ <- mapM (\(s, fs) -> do
              putStrLn $ "  " ++ s ++ ": " ++ (show $ length fs))
            strFilesPairs

  putStrLn ""
  putStrLn bar
  putStrLn "RM OUT OF PLACE"

  let badRmFiles =
        concatMap (\(f, Program exprs) ->
                      let exprs2 = dropFirstLast exprs in
                        if "LamApp (Var (Ident {idName = \"rm\""
                            `isInfixOf` (show exprs2) then [f] else [])
                  parsedPairs
  _ <- mapM (\f -> putStrLn $ "  " ++ f) badRmFiles
  putStrLn ""
  putStrLn $ "bad rm count: " ++ (show $ length badRmFiles)

  

  -- TEST: pure args
  -- putStrLn ""
  -- putStrLn bar
  -- let pureArgsPairs = map (\(f, p) -> (f, testPureArgCallFails p)) parsedPairs
  -- let numPureArgsPairs = length $ filter snd pureArgsPairs
  -- putStrLn $ "tpod: [pure args] " ++ show numPureArgsPairs ++ "/"
  --                                 ++ show numParsedPairs
  -- _ <- mapM (\(f, _) -> putStrLn $ "  " ++ f) $ filter snd pureArgsPairs

  -- Test the flagged ids used
  -- putStrLn ""
  -- putStrLn bar
  -- let fileIdsPairs = map (\(f, p) -> (f, testFlaggedIdsUsed p)) parsedPairs
  -- let fileIdPairsFlat = concat [map ((,) f) ids | (f, ids) <- fileIdsPairs]
  -- let idCountPairs = map (\i -> (i, length $ filter ((== i) . snd) fileIdPairsFlat)) flaggedIds

  -- putStrLn $ "tpod: flagged id tests"
  -- _ <- mapM_ (\(i, c) -> putStrLn $ "  " ++ show (idName i, c)) idCountPairs
  -- _ <- mapM_ (\(f, ids) -> do
  --                 putStrLn "--"
  --                 putStrLn f
  --                 putStrLn $ show $ map idName ids)
  --             fileIdsPairs

  putStrLn bar
  return ()

