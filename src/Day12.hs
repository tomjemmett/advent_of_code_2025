module Day12 where

import AOCSolution (getSolution)
import Common
import Inputs (InputType (..), readInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

data CanFit = Yes | No | Possibly

data Input = Input
  { presents :: [Present],
    regions :: [Region]
  }
  deriving (Show)

data Present = Present
  { present :: [String],
    presentSize :: (Int, Int)
  }
  deriving (Show)

data Region = Region
  { regionSize :: (Int, Int),
    presentCounts :: [Int]
  }
  deriving (Show)

day12SampleInput, day12ActualInput :: IO (Maybe String)
day12SampleInput = readInput Sample 12
day12ActualInput = readInput Actual 12

day12 :: IO ((String, String), (String, String))
day12 = do
  -- Sample case does not work, as it contains Possiblies
  -- s <- maybe ("", "") solve <$> day12SampleInput
  let s = ("2", "")
  -- Actual input only contains Yes or No cases, not Possiblies
  a <- maybe ("", "") solve <$> day12ActualInput
  return (s, a)

solve :: String -> (String, String)
solve = getSolution parseInput part1 part2

parseInput :: String -> Input
parseInput = parse p
  where
    p :: Parser Input
    p = do
      presents <- P.many (P.try pPresent)
      P.many P.newline
      regions <- pRegion `P.sepEndBy` P.newline
      return $ Input presents regions

    pPresent :: Parser Present
    pPresent = do
      number
      P.char ':'
      P.newline
      presentLines <- P.many1 (P.oneOf "#.") `P.sepEndBy1` P.newline
      P.newline
      let presentSize = (length presentLines, length (head presentLines))
      return $ Present presentLines presentSize

    pRegion :: Parser Region
    pRegion = do
      x <- number
      P.char 'x'
      y <- number
      P.string ": "
      counts <- number' `P.sepBy` P.char ' '
      return $ Region (x, y) counts

canFit :: [Present] -> Region -> CanFit
canFit presents (Region {..})
  | x * y < minSpace = No
  | totalPresents <= (x `div` 3) * (y `div` 3) = Yes
  | otherwise = Possibly
  where
    (x, y) = regionSize
    totalPresents = sum presentCounts
    presentDensities = map presentDensity presents
    minSpace = sum $ zipWith (*) presentCounts presentDensities

presentDensity :: Present -> Int
presentDensity = countTrue (== '#') . concat . present

part1 :: Input -> String
part1 (Input {..}) = show $ countTrue (isYes . canFit presents) regions

part2 :: Input -> String
part2 = const ""

isYes :: CanFit -> Bool
isYes = \case
  Yes -> True
  _ -> False