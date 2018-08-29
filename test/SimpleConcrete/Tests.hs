module SimpleConcrete.Tests where

import Test.Tasty
import Test.Tasty.HUnit

import TestUtils

import Lib

doPath :: String -> String
doPath file = "SimpleConcrete/test-files/" ++ file

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
            loadAndRunNoBase (doPath "vec-make.R") 1000 ==
              [Just (VecVal (IntVec [Atom 4444, Atom 5555, Atom 6666]))]

    , testCase "Vector length" $
        assertBool
          ("Expects [6]") $
            loadAndRunNoBase (doPath "vec-length.R") 1000 ==
              [Just (VecVal (IntVec [Atom 6]))]
    ]

