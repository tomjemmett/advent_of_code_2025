module Days
  ( day01,
    day02,
    day03,
    day04,
    day05,
    day06,
    day07,
    day08,
    day09,
    day10,
    day11,
    day12,
    runDay,
  )
where

import Control.Monad (when)
import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)
import Day05 (day05)
import Day06 (day06)
import Day07 (day07)
import Day08 (day08)
import Day09 (day09)
import Day10 (day10)
import Day11 (day11)
import Day12 (day12)
import Inputs (InputType (Actual, Sample), downloadInput, readInput)
import System.Directory (doesFileExist)
import System.TimeIt (timeIt)

days =
  [ day01,
    day02,
    day03,
    day04,
    day05,
    day06,
    day07,
    day08,
    day09,
    day10,
    day11,
    day12
  ]

runDay :: Int -> IO ()
runDay day = do
  let fn = days !! pred day

  ((s1, s2), (a1, a2)) <- fn
  when (s1 /= "") $ do
    putStrLn $ replicate 80 '-'
    putStr $ "Day: " ++ show day
    putStrLn ""

    putStrLn "Part 1 Sample:"
    putStr s1
    putStrLn ""

    when (s2 /= "") $ do
      putStrLn "Part 2 Sample:"
      putStrLn s2
      putStrLn ""

    when (a1 /= "") $ do
      putStrLn "Part 1 Actual:"
      putStr a1
      putStrLn ""

    when (a2 /= "") $ do
      putStrLn "Part 2 Actual:"
      putStrLn a2
      putStrLn ""
