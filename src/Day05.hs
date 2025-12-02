module Day05 where

import AOCSolution (getSolution)
import Common
import Inputs (InputType (..), readInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day05SampleInput, day05ActualInput :: IO (Maybe String)
day05SampleInput = readInput Sample 5
day05ActualInput = readInput Actual 5

day05 :: IO ((String, String), (String, String))
day05 = do
  s <- maybe ("", "") solve <$> day05SampleInput
  a <- maybe ("", "") solve <$> day05ActualInput
  return (s, a)

solve :: String -> (String, String)
solve = undefined
