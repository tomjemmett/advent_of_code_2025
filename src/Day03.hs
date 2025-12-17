module Day03 where

import AOCSolution (getSolution)
import Common
import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (maximumBy)
import Inputs (InputType (..), readInput)

day03SampleInput, day03ActualInput :: IO (Maybe String)
day03SampleInput = readInput Sample 3
day03ActualInput = readInput Actual 3

day03 :: IO ((String, String), (String, String))
day03 = do
  s <- maybe ("", "") solve <$> day03SampleInput
  a <- maybe ("", "") solve <$> day03ActualInput
  return (s, a)

-- >>> day03
-- (("357","3121910778619"),("17085","169408143086082"))

solve :: String -> (String, String)
solve = getSolution p (f 2) (f 12)
  where
    f n = sum . map (makeBest n)
    p = map (map digitToInt) . lines

makeBest :: Int -> [Int] -> Int
makeBest n vals = intListToInt $ take n $ go initN vals
  where
    initN = length vals - n + 1
    go :: Int -> [Int] -> [Int]
    go _ [] = []
    go n xs = v : go n' xs''
      where
        (v, xs') = splitAtMax (take n xs)
        n' = length xs' + 1
        xs'' = xs' ++ drop n xs

splitAtMax :: [Int] -> (Int, [Int])
splitAtMax xs = go (0, xs) xs
  where
    go (curMax, after) [] = (curMax, after)
    go (curMax, after) (y : ys)
      | y > curMax = go (y, ys) ys
      | otherwise = go (curMax, after) ys
