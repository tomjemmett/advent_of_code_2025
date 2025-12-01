module Day01 where

import Common
import Data.Bifunctor (bimap)
import Inputs (InputType (..), readInput)
import Text.Parsec qualified as P

day01SampleInput, day01ActualInput :: IO (Maybe String)
day01SampleInput = readInput Sample 1
day01ActualInput = readInput Actual 1

day01 :: IO ((String, String), (String, String))
day01 = do
  s <- maybe ("", "") solve <$> day01SampleInput
  a <- maybe ("", "") solve <$> day01ActualInput
  return (s, a)

solve :: String -> (String, String)
solve = bimap show show . fst . foldl rotate ((0, 0), 50) . parseInput

parseInput :: String -> [(Char, Int)]
parseInput = parse ((`P.sepEndBy` P.newline) ((,) <$> P.oneOf "LR" <*> number'))

rotate :: ((Int, Int), Int) -> (Char, Int) -> ((Int, Int), Int)
rotate ((p1, p2), pos) (dir, steps) = ((p1', p2'), pos')
  where
    dialSize = 100
    (n', pos') = case dir of
      'L' -> (pos - steps) `divMod` dialSize
      'R' -> (pos + steps) `divMod` dialSize
    adj
      | dir == 'R' = 0
      | pos == 0 = -1
      | pos' == 0 = 1
      | otherwise = 0
    p1' = (if pos' == 0 then succ else id) p1
    p2' = p2 + abs n' + adj
