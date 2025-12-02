module Day03 where

import AOCSolution (getSolution)
import Common
import Inputs (InputType (..), readInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day03SampleInput, day03ActualInput :: IO (Maybe String)
day03SampleInput = readInput Sample 3
day03ActualInput = readInput Actual 3

day03 :: IO ((String, String), (String, String))
day03 = do
  s <- maybe ("", "") solve <$> day03SampleInput
  a <- maybe ("", "") solve <$> day03ActualInput
  return (s, a)

solve :: String -> (String, String)
solve = undefined
