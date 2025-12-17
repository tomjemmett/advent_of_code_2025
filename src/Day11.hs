module Day11 where

import AOCSolution (getSolution)
import Common
import Control.Monad (forM)
import Control.Monad.State
import Data.HashMap.Strict qualified as M
import Inputs (InputType (..), readInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

day11SampleInput, day11ActualInput :: IO (Maybe String)
day11SampleInput = readInput Sample 11
day11ActualInput = readInput Actual 11

day11 :: IO ((String, String), (String, String))
day11 = do
  s <- maybe ("", "") solve <$> day11SampleInput
  a <- maybe ("", "") solve <$> day11ActualInput
  return (s, a)

-- >>> day11
-- (("5","2"),("428","331468292364745"))

solve :: String -> (String, String)
solve = getSolution parseInput part1 part2

parseInput :: String -> M.HashMap String [String]
parseInput = M.fromList . parse (pLine `P.sepEndBy` P.newline)
  where
    pLine :: Parser (String, [String])
    pLine = do
      from <- pName
      P.string ": "
      to <- pName `P.sepBy` P.char ' '
      pure (from, to)
    pName :: Parser String
    pName = P.many P.alphaNum

part1 :: M.HashMap String [String] -> Int
part1 = find 0 "you"

part2 :: M.HashMap String [String] -> Int
part2 = find 2 "svr"

find :: Int -> String -> M.HashMap String [String] -> Int
find v s i = evalState (go v s) M.empty
  where
    go :: Int -> String -> State (M.HashMap (Int, String) Int) Int
    go v "out" = pure $ fromEnum (v == 0)
    go v p = do
      s <- get
      case s M.!? (v, p) of
        Just n -> pure n
        Nothing -> do
          let ps = i M.! p
              v' = case p of
                "dac" -> v - 1
                "fft" -> v - 1
                _ -> v
          n <- sum <$> forM ps (go v')
          modify (M.insert (v, p) n)
          pure n
