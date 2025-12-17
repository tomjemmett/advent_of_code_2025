module Day08 where

import AOCSolution (getSolution)
import Common
import Control.Monad (when)
import Control.Monad.State
import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.List (sortBy)
import Data.PQueue.Prio.Min (MinPQueue ((:<)))
import Data.PQueue.Prio.Min qualified as PQ
import Inputs (InputType (..), readInput)

day08SampleInput, day08ActualInput :: IO (Maybe String)
day08SampleInput = readInput Sample 8
day08ActualInput = readInput Actual 8

day08 :: IO ((String, String), (String, String))
day08 = do
  s <- maybe ("", "") (solve 10) <$> day08SampleInput
  a <- maybe ("", "") (solve 1000) <$> day08ActualInput
  return (s, a)

-- >>> day08
-- (("40","25272"),("115885","274150525"))

solve :: Int -> String -> (String, String)
solve p1n = getSolution (run p1n . parseInput) fst snd

parseInput :: String -> [Point3d]
parseInput = map (tuplify3 . commaSeparatedInts) . lines

distance :: (Point3d, Point3d) -> Int
distance ((x1, y1, z1), (x2, y2, z2)) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2

getEdges :: [Point3d] -> PQ.MinPQueue Int (Point3d, Point3d)
getEdges points =
  PQ.fromList
    [ (d, p)
      | p1 <- points,
        p2 <- points,
        p1 < p2,
        let p = (p1, p2),
        let d = distance p,
        d < 15000 ^ 2 -- performance tweak, ignore anything "too" far away
    ]

run :: Int -> [Point3d] -> (Int, Int)
run p1N points = (p1, p2)
  where
    -- run the state for part 1 and get the union-find structure for part 2
    ((tgt, p1), ufP2) = runState (go (length points) (PQ.fromList edgesP1)) ufInit
    -- run the state for part 2
    p2 = snd $ evalState (go tgt edgesP2) ufP2
    -- split edges for part 1 and part 2
    (edgesP1, edgesP2) = PQ.splitAt p1N $ getEdges points
    -- initialize union-find structure
    ufInit = M.fromList [(p, p) | p <- points]
    go :: Int -> PQ.MinPQueue Int (Point3d, Point3d) -> State (M.HashMap Point3d Point3d) (Int, Int)
    -- specific case for part 1, we have used up all of the edges
    go tgt q | PQ.null q = (tgt,) . p1Answer <$> forM points unionFind
    go tgt ((_, (p1, p2)) :< ps) = do
      p1f <- unionFind p1
      p2f <- unionFind p2
      let tgt' = if p1f == p2f then tgt else pred tgt
      if tgt' == 1
        then return (0, p2Answer p1 p2)
        else do
          when (p1f /= p2f) $ mix p1 p2
          go tgt' ps
    p1Answer :: [Point3d] -> Int
    p1Answer =
      product
        . take 3
        . map snd
        . sortBy (flip compare `on` snd)
        . M.toList
        . M.fromListWith (+)
        . map (,1)
    p2Answer :: Point3d -> Point3d -> Int
    p2Answer (x1, _, _) (x2, _, _) = x1 * x2

unionFind :: Point3d -> State (M.HashMap Point3d Point3d) Point3d
unionFind i = do
  uf <- get
  let j = uf M.! i
  if j == i
    then return i
    else do
      r <- unionFind j
      modify (M.insert i r)
      return r

mix :: Point3d -> Point3d -> State (M.HashMap Point3d Point3d) ()
mix i j = do
  ri <- unionFind i
  rj <- unionFind j
  modify (M.insert ri rj)
