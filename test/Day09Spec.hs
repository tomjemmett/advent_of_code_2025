module Day09Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 9" $ do
  (sample, actual) <- runIO day09

  it "Sample" $ do
    sample `shouldBe` ("50", "24")

  it "Actual" $ do
    actual `shouldBe` ("4777409595", "1473551379")
