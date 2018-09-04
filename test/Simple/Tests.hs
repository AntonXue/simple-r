module Simple.Tests where

import Test.Tasty
import Test.Tasty.HUnit

import TestUtils

import Lib

doPath :: String -> String
doPath file = "/home/celery/foo/harvard/simple-r/test/Simple/test-files/" ++ file

defN :: Int
defN = 1000

tests :: TestTree
tests =
  testGroup "Simple Concrete"
    [ testCase "Should always pass" $
        assertBool
          ("This shouldn't have failed!") $
          True

    , testCase "Vector creation" $
        assertBool
          ("Expects [4444, 5555, 6666]") $
            loadAndRunNoBase (doPath "vec-make.R") defN ==
              [Just (VecVal (IntVec [Atom 4444, Atom 5555, Atom 6666]))]

    , testCase "Vector length" $
        assertBool
          ("Expects [6]") $
            loadAndRunNoBase (doPath "vec-length.R") defN ==
              [Just (VecVal (IntVec [Atom 6]))]

    , testCase "Vector colon" $
        assertBool
          ("Expects [3, 4, 5, 6, 7, 8, 9]") $
            loadAndRunNoBase (doPath "vec-colon.R") defN ==
            [Just (VecVal (IntVec (map Atom [3, 4, 5, 6, 7, 8, 9])))]

    , testCase "Vector plus" $
        assertBool
          ("Expects [6666, 8888, 11110, 9999]") $
            loadAndRunNoBase (doPath "vec-plus.R") defN ==
            [Just (VecVal (IntVec (map Atom [6666, 8888, 11110, 9999])))]

    , testCase "Vector arithmetics (no div)" $
        assertBool
          ("Expects [24685309, 44433334, 69118643, 69120865, 44433334]") $
            loadAndRunNoBase (doPath "vec-arith.R") defN ==
            [Just (VecVal (IntVec (map Atom
                      [24685309, 44433334, 69118643, 69120865, 44433334])))]
    ]

