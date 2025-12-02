module Day02 where

import AOCSolution (getSolution)
import Common
import Data.List (splitAt)
import Data.List.Split (chunksOf)
import Inputs (InputType (..), readInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

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

part :: (Int -> Bool) -> [(Int, Int)] -> Int
part isInvalid = sum . map fn
  where
    fn (s, e) = sum $ filter isInvalid [s .. e]

part1 :: [(Int, Int)] -> Int
part1 = part isInvalid
  where
    isInvalid (show -> s) = stringRepeatsN s ((length s + 1) `div` 2)

part2 :: [(Int, Int)] -> Int
part2 = part isInvalid
  where
    isInvalid (show -> s) = any (stringRepeatsN s) [1 .. length s `div` 2]

stringRepeatsN :: String -> Int -> Bool
stringRepeatsN s n = length xs > 1 && all (== head xs) (tail xs)
  where
    xs = chunksOf n s

parseInput :: String -> [(Int, Int)]
parseInput = parse (((,) <$> (number <* P.char '-') <*> number) `P.sepBy` P.char ',')