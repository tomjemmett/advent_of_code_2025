module Day02 where

import Common
import Inputs (InputType (..), readInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day02SampleInput, day02ActualInput :: IO (Maybe String)
day02SampleInput = readInput Sample 1
day02ActualInput = readInput Actual 1

day02 :: IO ((String, String), (String, String))
day02 = do
  s <- maybe ("", "") solve <$> day02SampleInput
  a <- maybe ("", "") solve <$> day02ActualInput
  return (s, a)

solve :: String -> (String, String)
solve = undefined
