module Day07 where

import AOCSolution (getSolution)
import Common
import Control.Monad (forM, guard)
import Control.Monad.RWS
import Data.HashMap.Strict qualified as M
import Data.Set qualified as S
import Data.Vector qualified as V
import Inputs (InputType (..), readInput)

day07SampleInput, day07ActualInput :: IO (Maybe String)
day07SampleInput = readInput Sample 7
day07ActualInput = readInput Actual 7

day07 :: IO ((String, String), (String, String))
day07 = do
  s <- maybe ("", "") solve <$> day07SampleInput
  a <- maybe ("", "") solve <$> day07ActualInput
  return (s, a)

solve :: String -> (String, String)
solve = getSolution (runTachyons . parseInput) fst snd

parseInput :: String -> V.Vector [Int]
parseInput = V.fromList . map f . lines
  where
    f = map fst . filter ((/= '.') . snd) . zip [0 ..]

runTachyons :: V.Vector [Int] -> (Int, Int)
runTachyons splitters = (S.size visited, total)
  where
    start = head $ splitters V.! 0
    (total, visited) = evalRWS (moveBeam (start, 1)) splitters M.empty

moveBeam :: (Int, Int) -> RWS (V.Vector [Int]) (S.Set (Int, Int)) (M.HashMap (Int, Int) Int) Int
moveBeam (x, y) = do
  splitters <- ask
  if y >= V.length splitters
    then pure 1
    else do
      visited <- get
      case M.lookup (x, y) visited of
        Just v -> pure v
        Nothing -> do
          let xs = splitters V.! y
          res <-
            if x `elem` xs
              then do
                tell $ S.singleton (x, y)
                sum <$> forM [pred x, succ x] (\nx -> moveBeam (nx, succ y))
              else moveBeam (x, succ y)
          modify $ M.insert (x, y) res
          pure res
