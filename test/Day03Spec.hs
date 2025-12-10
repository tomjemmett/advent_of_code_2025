module Day03Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 3" $ do
  (sample, actual) <- runIO day03

  it "Sample" $ do
    sample `shouldBe` ("357", "3121910778619")

  it "Actual" $ do
    actual `shouldBe` ("17085", "169408143086082")
