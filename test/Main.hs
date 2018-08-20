module Main where

import Test.Tasty
import Test.Tasty.HUint

main :: IO ()
main = do
  defaultMain tests
  putStrLn "test/main: done!"

tests :: TestTree
tests = testGroup "Tests" []


