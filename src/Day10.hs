module Day10 where

import AOCSolution (getSolution)
import Common
import Inputs (InputType (..), readInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day10SampleInput, day10ActualInput :: IO (Maybe String)
day10SampleInput = readInput Sample 10
day10ActualInput = readInput Actual 10

day10 :: IO ((String, String), (String, String))
day10 = do
  s <- maybe ("", "") solve <$> day10SampleInput
  a <- maybe ("", "") solve <$> day10ActualInput
  return (s, a)

solve :: String -> (String, String)
solve = undefined
