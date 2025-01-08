module Day01 where

import Common
import Inputs (InputType (..), readInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day01SampleInput, day01ActualInput :: IO (Maybe String)
day01SampleInput = readInput Sample 1
day01ActualInput = readInput Actual 1

day01 :: IO ((String, String), (String, String))
day01 = do
  s <- maybe ("", "") solve <$> day01SampleInput
  a <- maybe ("", "") solve <$> day01ActualInput
  return (s, a)

solve :: String -> (String, String)
solve = undefined
