module Day08 where

import Common
import Inputs (InputType (..), readInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day08SampleInput, day08ActualInput :: IO (Maybe String)
day08SampleInput = readInput Sample 1
day08ActualInput = readInput Actual 1

day08 :: IO ((String, String), (String, String))
day08 = do
  s <- maybe ("", "") solve <$> day08SampleInput
  a <- maybe ("", "") solve <$> day08ActualInput
  return (s, a)

solve :: String -> (String, String)
solve = undefined
