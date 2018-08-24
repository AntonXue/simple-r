module TestUtils where

import System.Directory
import System.IO.Unsafe -- HAHAHAHA

import Lib


loadAndRunNoBase :: String -> Int -> [Maybe Value]
loadAndRunNoBase userFile ticks =
  let absFile = "test/" ++ userFile in
  let state = unsafePerformIO $ loadUserGuessWithNoBase absFile in
    getValuesRunN ticks state



