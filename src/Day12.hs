module Day12 where

import AOCSolution (getSolution)
import Common
import Inputs (InputType (..), readInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day12SampleInput, day12ActualInput :: IO (Maybe String)
day12SampleInput = readInput Sample 12
day12ActualInput = readInput Actual 12

day12 :: IO ((String, String), (String, String))
day12 = do
  s <- maybe ("", "") solve <$> day12SampleInput
  a <- maybe ("", "") solve <$> day12ActualInput
  return (s, a)

solve :: String -> (String, String)
solve = undefined
