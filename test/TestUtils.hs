module TestUtils where

import System.Directory
import System.IO.Unsafe -- HAHAHAHA

import Lib

rmdTmpFile :: String
rmdTmpFile = "/home/celery/foo/harvard/simple-r/~rmd.tmp"

loadAndRunNoBase :: String -> Int -> [Maybe Value]
loadAndRunNoBase userFile ticks =
  let absFile = userFile in
  let state = unsafePerformIO $ loadUserGuessWithNoBase absFile in
    getValuesRunN ticks state



