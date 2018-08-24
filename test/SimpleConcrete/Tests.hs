module SimpleConcrete.Tests where

import Test.Tasty
import Test.Tasty.HUnit

import TestUtils

import Lib



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
            loadAndRunNoBase "SimpleConcrete/test-files/vec-make.R" 1000 ==
              [Just (VecVal (IntVec [Atom 4444, Atom 5555, Atom 6666]))]

    ]

