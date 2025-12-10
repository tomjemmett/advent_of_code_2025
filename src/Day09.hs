module Day09 where

import AOCSolution (getSolution)
import Common
import Data.Function (on)
import Data.List (find, sortBy)
import Data.Maybe (fromJust)
import Inputs (InputType (..), readInput)

data Input = Input
  { path :: [Point2d],
    rectangles :: [((Point2d, Point2d), Int)]
  }
  deriving (Show)

mkInput :: [Point2d] -> Input
mkInput pts = Input (pts ++ take 1 pts) (getAllRectangles pts)

day09SampleInput, day09ActualInput :: IO (Maybe String)
day09SampleInput = readInput Sample 9
day09ActualInput = readInput Actual 9

day09 :: IO ((String, String), (String, String))
day09 = do
  s <- maybe ("", "") solve <$> day09SampleInput
  a <- maybe ("", "") solve <$> day09ActualInput
  return (s, a)

solve :: String -> (String, String)
solve = getSolution parseInput part1 part2

parseInput :: String -> Input
parseInput = mkInput . map (tuplify2 . commaSeparatedInts) . lines

getAllRectangles :: [Point2d] -> [((Point2d, Point2d), Int)]
getAllRectangles points =
  sortBy
    (flip compare `on` snd)
    [ ((p1, p2), getRectangleAreas p1 p2)
      | p1 <- points,
        p2 <- points,
        p1 < p2
    ]

getRectangleAreas :: Point2d -> Point2d -> Int
getRectangleAreas (x1, y1) (x2, y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

part1 :: Input -> Int
part1 = snd . head . rectangles

part2 :: Input -> Int
part2 (Input {..}) = snd $ fromJust $ find insidePath rectangles
  where
    pathSegments = zipWith getBounds path (tail path)
    getBounds (x1, y1) (x2, y2) =
      let (xmin, xmax) = if x1 < x2 then (x1, x2) else (x2, x1)
          (ymin, ymax) = if y1 < y2 then (y1, y2) else (y2, y1)
       in (xmin, xmax, ymin, ymax)
    insidePath :: ((Point2d, Point2d), Int) -> Bool
    insidePath (rect, _) = all checkLine pathSegments
      where
        (rectXmin, rectXmax, rectYmin, rectYmax) = uncurry getBounds rect
        checkLine (lineXmin, lineXmax, lineYmin, lineYmax) =
          or
            [ lineXmax <= rectXmin,
              rectXmax <= lineXmin,
              lineYmax <= rectYmin,
              rectYmax <= lineYmin
            ]
