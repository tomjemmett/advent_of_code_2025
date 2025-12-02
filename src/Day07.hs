module Day07 where

import AOCSolution (getSolution)
import Common
import Inputs (InputType (..), readInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day07SampleInput, day07ActualInput :: IO (Maybe String)
day07SampleInput = readInput Sample 7
day07ActualInput = readInput Actual 7

day07 :: IO ((String, String), (String, String))
day07 = do
  s <- maybe ("", "") solve <$> day07SampleInput
  a <- maybe ("", "") solve <$> day07ActualInput
  return (s, a)

solve :: String -> (String, String)
solve = undefined
