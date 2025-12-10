{-# LANGUAGE InstanceSigs #-}

module AOCSolution where

data PartResult = StringResult String | IntResult Int

instance Show PartResult where
  show :: PartResult -> String
  show = \case
    StringResult x -> x
    IntResult x -> show x

class ToPartResult a where
  toPartResult :: a -> PartResult

instance ToPartResult String where
  toPartResult :: String -> PartResult
  toPartResult = StringResult

instance ToPartResult Int where
  toPartResult :: Int -> PartResult
  toPartResult = IntResult

instance ToPartResult Integer where
  toPartResult :: Integer -> PartResult
  toPartResult = IntResult . fromIntegral

getSolution :: (ToPartResult b) => (String -> a) -> (a -> b) -> (a -> b) -> String -> (String, String)
getSolution parser part1 part2 input =
  let parsed = parser input
   in (show $ toPartResult $ part1 parsed, show $ toPartResult $ part2 parsed)
