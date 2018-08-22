-- Implicitly the Main module for tests

import Test.Tasty

import TestUtils

import qualified SimpleConcrete.Tests as SimpleConcrete

main :: IO ()
main = do
  defaultMain tests
  putStrLn "test/main: done!"

tests :: TestTree
tests =
  testGroup "Tests"
    [ SimpleConcrete.tests

    ]


