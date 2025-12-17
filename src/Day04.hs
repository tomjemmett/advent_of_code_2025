module Day04 where

import AOCSolution (getSolution)
import Common
import Data.Massiv.Array (Ix2 (..))
import Data.Massiv.Array qualified as A
import Inputs (InputType (..), readInput)

day04SampleInput, day04ActualInput :: IO (Maybe String)
day04SampleInput = readInput Sample 4
day04ActualInput = readInput Actual 4

day04 :: IO ((String, String), (String, String))
day04 = do
  s <- maybe ("", "") solve <$> day04SampleInput
  a <- maybe ("", "") solve <$> day04ActualInput
  return (s, a)

-- >>> day04
-- (("13","43"),("1457","8310"))

solve :: String -> (String, String)
solve = getSolution getIterations part1 part2

part1 :: [Int] -> Int
part1 = head

part2 :: [Int] -> Int
part2 = sum . takeWhile (> 0)

getIterations :: String -> [Int]
getIterations input = zipWith (-) iters (tail iters)
  where
    i = A.fromLists' A.Par . map2 (fromEnum . (== '@')) . lines $ input
    iters = map A.sum $ iterate update i

update :: (A.Manifest r Int) => A.Array r A.Ix2 Int -> A.Array A.U A.Ix2 Int
update = A.computeP . A.mapStencil (A.Fill 0) stencil
  where
    stencil = A.makeStencil (A.Sz (3 :. 3)) (1 :. 1) iter
    iter get
      | get (0 :. 0) == 0 = 0
      | sum s < 4 = 0
      | otherwise = 1
      where
        s = [get (y :. x) | y <- [-1 .. 1], x <- [-1 .. 1], (y, x) /= (0, 0)]
