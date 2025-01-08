module SpecHelper
  ( module Test.Hspec,
    module Days,
    module System.IO,
  )
where

import Days
import System.IO (IOMode (ReadMode), hGetContents, withFile)
import Test.Hspec
