module Day04 where

import AOCSolution (getSolution)
import Common
import Inputs (InputType (..), readInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day04SampleInput, day04ActualInput :: IO (Maybe String)
day04SampleInput = readInput Sample 4
day04ActualInput = readInput Actual 4

day04 :: IO ((String, String), (String, String))
day04 = do
  s <- maybe ("", "") solve <$> day04SampleInput
  a <- maybe ("", "") solve <$> day04ActualInput
  return (s, a)

solve :: String -> (String, String)
solve = undefined
