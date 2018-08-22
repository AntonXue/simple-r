module SimpleConcrete.Tests where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup "Simple Concrete"
    [ testCase "Should always pass" $
        assertBool
          ("This shouldn't have failed!")
          True
    ]

