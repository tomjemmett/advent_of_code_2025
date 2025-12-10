module Day05Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 5" $ do
  (sample, actual) <- runIO day05

  it "Sample" $ do
    sample `shouldBe` ("3", "14")

  it "Actual" $ do
    actual `shouldBe` ("862", "357907198933892")
