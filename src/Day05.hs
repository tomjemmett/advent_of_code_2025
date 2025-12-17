module Day05 where

import AOCSolution (getSolution)
import Common
import Data.List (sort)
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

-- >>> day05
-- (("3","14"),("862","357907198933892"))

solve :: String -> (String, String)
solve = getSolution parseInput part1 part2

parseInput :: String -> ([(Int, Int)], [Int])
parseInput = parse p
  where
    p = do
      intervals <- collapse . sort <$> pInterval `P.sepEndBy` P.newline
      P.newline
      numbers <- number' `P.sepEndBy` P.newline
      return (intervals, numbers)
    pInterval = (,) <$> (number' <* P.char '-') <*> number'

part1 :: ([(Int, Int)], [Int]) -> Int
part1 (intervals, values) = countTrue inInterval values
  where
    inInterval x = any (\(lo, hi) -> lo <= x && x <= hi) intervals

part2 :: ([(Int, Int)], [Int]) -> Int
part2 = sum . map (succ . uncurry subtract) . fst

collapse :: [(Int, Int)] -> [(Int, Int)]
collapse is | length is <= 1 = is
collapse ((lo1, hi1) : (lo2, hi2) : is) =
  if hi1 < lo2
    then (lo1, hi1) : collapse ((lo2, hi2) : is)
    else collapse $ (lo1, max hi1 hi2) : is
