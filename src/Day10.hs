module Day10 where

import AOCSolution (getSolution)
import Common
import Control.Monad (forM, forM_)
import Data.SBV (OptimizeResult (LexicographicResult), OptimizeStyle (Lexicographic), constrain, getModelValue, minimize, optimize, sInteger, sMod, (.==), (.>=))
import Inputs (InputType (..), readInput)
import Text.Parsec qualified as P
import Text.Parsec.String (Parser)

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

-- >>> day10
-- (("7","33"),("432","18011"))

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
part constraintFn modValue = fmap sum . mapM goSBV
  where
    goSBV :: Input -> IO Int
    goSBV input =
      optimize Lexicographic do
        let btns = buttons input
            num = length btns
        vars <- forM [1 .. num] $ \n -> sInteger ("x" ++ show n)

        forM_ vars $ \v -> constrain $ v .>= 0

        forM_ (zip [0 ..] $ constraintFn input) $ \(i, target) -> do
          let sumExpr = sum $ map fst $ filter (elem i . snd) $ zip vars btns
              finalExpr = case modValue of
                Just n -> sumExpr `sMod` fromIntegral n
                Nothing -> sumExpr
          constrain $ finalExpr .== fromIntegral target

        let totalPresses = sum vars
        minimize "total_presses" totalPresses
        >>= \case
          LexicographicResult model ->
            case getModelValue "total_presses" model :: Maybe Integer of
              Just total -> pure $ fromIntegral total
              Nothing -> error $ "No solution found for input: " ++ show input
          _ -> error $ "No solution found for input: " ++ show input
