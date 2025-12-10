{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import AOCSolution (getSolution)
import Algorithm.Search
import Common
import Control.Monad (forM, forM_, unless, (<=<))
import Data.Bits (xor)
import Data.Maybe (catMaybes, fromJust)
import Data.Traversable qualified as T
import Inputs (InputType (..), readInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)
import Z3.Base

data Input = Input
  { lights :: Int,
    buttons :: [[Int]],
    jolts :: [Int]
  }
  deriving (Show)

day10SampleInput, day10ActualInput :: IO (Maybe String)
day10SampleInput = readInput Sample 10
day10ActualInput = readInput Actual 10

day10 :: IO ((String, String), (String, String))
day10 = do
  s <- solve <$> day10SampleInput
  a <- solve <$> day10ActualInput
  (,) <$> s <*> a

solve :: Maybe String -> IO (String, String)
solve input = case input of
  Nothing -> return ("", "")
  Just input -> do
    let i = parseInput input
    p1 <- part1 i
    p2 <- part2 i
    return (show p1, show p2)

parseInput :: String -> [Input]
parseInput = parse (p `P.sepEndBy` P.newline)
  where
    p :: Parser Input
    p = do
      light <- P.between (P.char '[') (P.char ']') (P.many $ P.oneOf ".#")
      P.char ' '
      buttons <- pButton `P.sepEndBy` P.char ' '
      jolts <- P.between (P.char '{') (P.char '}') pNums
      pure $ mkInput light buttons jolts
    pButton :: Parser [Int]
    pButton = P.between (P.char '(') (P.char ')') pNums
    pNums :: Parser [Int]
    pNums = number `P.sepBy` P.char ','

mkInput :: String -> [[Int]] -> [Int] -> Input
mkInput light = Input light'
  where
    light' = lightStringToInt light

lightStringToInt :: String -> Int
lightStringToInt = foldl f 0 . reverse
  where
    f acc = \case
      '.' -> acc * 2
      '#' -> acc * 2 + 1

part1 :: [Input] -> IO Int
part1 = pure . sum . map go
  where
    go (Input {..}) = fst $ fromJust $ dijkstra nfn (const . const 1) (== lights) 0
      where
        buttons' = map (sum . map (2 ^)) buttons
        nfn :: Int -> [Int]
        nfn s = map (xor s) buttons'

part2 :: [Input] -> IO Int
part2 inputs = sum . map fromJust <$> forM inputs goZ3
  where
    goZ3 :: Input -> IO (Maybe Int)
    goZ3 (Input {..}) = do
      cfg <- mkConfig
      ctx <- mkContext cfg
      opt <- mkOptimize ctx

      -- Create integer variables
      let numButtons = length buttons
      btns <- forM [1 .. numButtons] (\i -> mkFreshIntVar ctx ('b' : show i))

      -- Each press >= 0
      zero <- mkInteger ctx 0
      forM_ btns $ optimizeAssert ctx opt <=< flip (mkGe ctx) zero

      -- For each joltage constraint
      forM_ (zip [0 ..] jolts) $ \(i, joltage) -> do
        let relevantPresses = [press | (press, button) <- zip btns buttons, i `elem` button]
        unless (null relevantPresses) $ do
          sumExpr <- mkAdd ctx relevantPresses
          targetExpr <- mkInteger ctx (fromIntegral joltage)
          eq <- mkEq ctx sumExpr targetExpr
          optimizeAssert ctx opt eq

      -- Minimize sum of presses
      totalPresses <- mkAdd ctx btns
      optimizeMinimize ctx opt totalPresses

      -- Check and get solution
      result <- optimizeCheck ctx opt []
      if result == Sat
        then do
          model <- optimizeGetModel ctx opt
          values <- sequence <$> mapM (evalInt ctx model) btns
          return $ sum . map fromIntegral <$> values
        else return Nothing
