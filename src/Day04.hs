module Day04 where

import AOCSolution (getSolution)
import Common
import Control.Monad (guard)
import Data.HashSet qualified as S
import Inputs (InputType (..), readInput)

day04SampleInput, day04ActualInput :: IO (Maybe String)
day04SampleInput = readInput Sample 4
day04ActualInput = readInput Actual 4

day04 :: IO ((String, String), (String, String))
day04 = do
  s <- maybe ("", "") solve <$> day04SampleInput
  a <- maybe ("", "") solve <$> day04ActualInput
  return (s, a)

solve :: String -> (String, String)
solve = getSolution parseInput part1 part2

parseInput :: String -> S.HashSet Point2d
parseInput input = S.fromList do
  (i, line) <- zip [0 ..] $ lines input
  (j, v) <- zip [0 ..] line
  guard $ v == '@'
  pure (i, j)

canRemove :: S.HashSet Point2d -> S.HashSet Point2d
canRemove points = S.filter ((< 4) . countNeighbors) points
  where
    countNeighbors :: Point2d -> Int
    countNeighbors = length . filter (`S.member` points) . point2dNeighboursDiags

part1 :: S.HashSet Point2d -> Int
part1 = S.size . canRemove

part2 :: S.HashSet Point2d -> Int
part2 = go
  where
    go p =
      if S.null rm
        then 0
        else S.size rm + go p'
      where
        rm = canRemove p
        p' = S.difference p rm