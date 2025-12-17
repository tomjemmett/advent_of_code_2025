module Day06 where

import AOCSolution (getSolution)
import Common
import Data.Bifunctor (second)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Inputs (InputType (..), readInput)

data Op = Multiply | Add deriving (Show)

day06SampleInput, day06ActualInput :: IO (Maybe String)
day06SampleInput = readInput Sample 6
day06ActualInput = readInput Actual 6

day06 :: IO ((String, String), (String, String))
day06 = do
  s <- maybe ("", "") solve <$> day06SampleInput
  a <- maybe ("", "") solve <$> day06ActualInput
  return (s, a)

-- >>> day06
-- (("4277556","3263827"),("6635273135233","12542543681221"))

solve :: String -> (String, String)
solve = getSolution parseInput part1 part2

parseInput :: String -> [(Op, [String])]
parseInput input = parseOp (init i) (last i ++ " ")
  where
    i = lines input

parseOp :: [String] -> String -> [(Op, [String])]
parseOp _ [] = []
parseOp nums (o : ops) = (op, ns) : parseOp nums' ops'
  where
    n = length $ takeWhile (== ' ') ops
    op = getOp [o]
    ops' = drop n ops
    nums' = map (drop $ succ n) nums
    ns = map (take n) nums

part1 :: [(Op, [String])] -> Int
part1 = solution . map (second f)
  where
    f = map (read . filter (/= ' '))

part2 :: [(Op, [String])] -> Int
part2 = solution . map (second makeNums)
  where
    makeNums :: [String] -> [Int]
    makeNums xs | all (== "") xs = []
    makeNums xs = i : makeNums (map tail xs)
      where
        i = read $ filter (/= ' ') $ map head xs

runOp :: Op -> [Int] -> Int
runOp Multiply = product
runOp Add = sum

solution :: [(Op, [Int])] -> Int
solution = sum . map (uncurry runOp)

getOp :: String -> Op
getOp = \case
  "*" -> Multiply
  "+" -> Add
