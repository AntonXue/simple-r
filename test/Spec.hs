-- Implicitly the Main module for tests

import Test.Tasty

import TestUtils

import qualified Simple.Tests as Simple

main :: IO ()
main = do
  defaultMain tests
  putStrLn "test/main: done!"

tests :: TestTree
tests =
  testGroup "Tests"
    [ Simple.tests

    ]


