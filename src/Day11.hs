module Day11 where

import Common
import Inputs (InputType (..), readInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day11SampleInput, day11ActualInput :: IO (Maybe String)
day11SampleInput = readInput Sample 1
day11ActualInput = readInput Actual 1

day11 :: IO ((String, String), (String, String))
day11 = do
  s <- maybe ("", "") solve <$> day11SampleInput
  a <- maybe ("", "") solve <$> day11ActualInput
  return (s, a)

solve :: String -> (String, String)
solve = undefined
