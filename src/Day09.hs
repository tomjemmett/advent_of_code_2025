module Day09 where

import AOCSolution (getSolution)
import Common
import Inputs (InputType (..), readInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day09SampleInput, day09ActualInput :: IO (Maybe String)
day09SampleInput = readInput Sample 9
day09ActualInput = readInput Actual 9

day09 :: IO ((String, String), (String, String))
day09 = do
  s <- maybe ("", "") solve <$> day09SampleInput
  a <- maybe ("", "") solve <$> day09ActualInput
  return (s, a)

solve :: String -> (String, String)
solve = undefined
