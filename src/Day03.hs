module Day03 where

import AOCSolution (getSolution)
import Common
import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (maximumBy, tails)
import Inputs (InputType (..), readInput)
import Text.Parsec qualified as P

day03SampleInput, day03ActualInput :: IO (Maybe String)
day03SampleInput = readInput Sample 3
day03ActualInput = readInput Actual 3

day03 :: IO ((String, String), (String, String))
day03 = do
  s <- maybe ("", "") solve <$> day03SampleInput
  a <- maybe ("", "") solve <$> day03ActualInput
  return (s, a)

solve :: String -> (String, String)
solve = getSolution p (f 2) (f 12)
  where
    f n = sum . map (makeBest n)
    p = map (map digitToInt) . lines

makeBest :: Int -> [Int] -> Int
makeBest n vals = snd $ iterate go (init, 0) !! n
  where
    init = replicate (length vals) False
    go (ons, _) = maximumBy (compare `on` snd) $ map f ons'
      where
        tests = cycle $ take (length ons) $ True : repeat False
        ons' = map (\i -> zipWith (||) (drop i tests) ons) [0 .. length ons - 1]
        f o = (o, intListToInt $ map snd $ filter fst $ zip o vals)