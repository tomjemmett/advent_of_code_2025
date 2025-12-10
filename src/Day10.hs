module Day10 where

import AOCSolution (getSolution)
import Common
import Control.Monad (forM, forM_, (<=<))
import Data.Maybe (fromJust)
import Inputs (InputType (..), readInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)
import Z3.Base

data Input = Input
  { buttons :: [[Int]],
    lights :: [Int],
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
    p1 <- part lights (Just 2) i
    p2 <- part jolts Nothing i
    return (show p1, show p2)

parseInput :: String -> [Input]
parseInput = parse (p `P.sepEndBy` P.newline)
  where
    p :: Parser Input
    p = do
      light <- pLight
      P.char ' '
      buttons <- pButton `P.sepEndBy` P.char ' '
      jolts <- between '{' '}' pNums
      pure $ Input buttons light jolts
    between :: Char -> Char -> Parser a -> Parser a
    between s e = P.between (P.char s) (P.char e)
    pLight :: Parser [Int]
    pLight = do
      xs <- between '[' ']' (P.many $ P.oneOf ".#")
      pure $ map (\case '#' -> 1; _ -> 0) xs
    pButton :: Parser [Int]
    pButton = between '(' ')' pNums
    pNums :: Parser [Int]
    pNums = number `P.sepBy` P.char ','

part :: (Input -> [Int]) -> Maybe Integer -> [Input] -> IO Int
part constraintFn modValue = fmap sum . mapM goZ3
  where
    goZ3 :: Input -> IO Int
    goZ3 input = do
      cfg <- mkConfig
      ctx <- mkContext cfg
      opt <- mkOptimize ctx

      -- Create integer variables
      let btns = buttons input
          num = length btns
      vars <- forM [1 .. num] $ mkFreshIntVar ctx . ('x' :) . show

      -- Each press >= 0
      zero <- mkInteger ctx 0
      forM_ vars $ optimizeAssert ctx opt <=< flip (mkGe ctx) zero

      -- For each constraint
      forM_ (zip [0 ..] $ constraintFn input) $ \(i, target) -> do
        sumExpr <- mkAdd ctx $ map fst $ filter (elem i . snd) $ zip vars btns

        -- for part 1, we are modulo 2, for part 2 any integer is valid
        finalExpr <- case modValue of
          Just n -> do
            modN <- mkInteger ctx n
            mkMod ctx sumExpr modN
          Nothing -> return sumExpr

        targetExpr <- mkInteger ctx (fromIntegral target)
        eq <- mkEq ctx finalExpr targetExpr
        optimizeAssert ctx opt eq

      -- Minimize sum of presses
      totalPresses <- mkAdd ctx vars
      optimizeMinimize ctx opt totalPresses

      -- Check and get solution
      result <- optimizeCheck ctx opt []
      if result == Sat
        then do
          model <- optimizeGetModel ctx opt
          values <- sequence <$> mapM (evalInt ctx model) vars
          return $ fromIntegral $ sum $ fromJust values
        else error $ "No solution found for input: " ++ show input
