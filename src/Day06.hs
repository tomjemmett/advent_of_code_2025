module Day06 where

import AOCSolution (getSolution)
import Common
import Inputs (InputType (..), readInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day06SampleInput, day06ActualInput :: IO (Maybe String)
day06SampleInput = readInput Sample 6
day06ActualInput = readInput Actual 6

day06 :: IO ((String, String), (String, String))
day06 = do
  s <- maybe ("", "") solve <$> day06SampleInput
  a <- maybe ("", "") solve <$> day06ActualInput
  return (s, a)

solve :: String -> (String, String)
solve = undefined
