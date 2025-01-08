module Day10Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 10" $ do
  (sample, actual) <- runIO day10

  it "Sample" $ do
    sample `shouldBe` ("", "")

  it "Actual" $ do
    actual `shouldBe` ("", "")
