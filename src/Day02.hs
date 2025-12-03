module Day02 where

import AOCSolution (getSolution)
import Common
import Data.List (nub)
import Inputs (InputType (..), readInput)
import Text.Parsec qualified as P

day02SampleInput, day02ActualInput :: IO (Maybe String)
day02SampleInput = readInput Sample 2
day02ActualInput = readInput Actual 2

day02 :: IO ((String, String), (String, String))
day02 = do
  s <- maybe ("", "") solve <$> day02SampleInput
  a <- maybe ("", "") solve <$> day02ActualInput
  return (s, a)

solve :: String -> (String, String)
solve = getSolution parseInput part1 part2

part1 :: [(Int, Int)] -> Int
part1 = sum . map (findRepeats $ take 1)

part2 :: [(Int, Int)] -> Int
part2 = sum . map (findRepeats id)

parseInput :: String -> [(Int, Int)]
parseInput = parse (((,) <$> (number <* P.char '-') <*> number) `P.sepBy` P.char ',')

findRepeats :: ([Int] -> [Int]) -> (Int, Int) -> Int
findRepeats dfn (lo, hi) = sum . nub . concat $ do
  let digits = floor (logBase 10 (fromIntegral hi)) + 1
  d <- [1 .. digits]
  r <- dfn [2 .. (digits `div` d) + 1]
  let p10d = 10 ^ d
      p10dr = 10 ^ (d * r)
      f = (p10dr - 1) `div` (p10d - 1)
      k_lo = max ((lo + f - 1) `div` f) (10 ^ (d - 1))
      k_hi = min (hi `div` f) (p10d - 1)
  pure $ filter ((>= lo) <&> (<= hi)) $ map (* f) [k_lo .. k_hi]