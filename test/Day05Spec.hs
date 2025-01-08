module Day05Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 5" $ do
  (sample, actual) <- runIO day05

  it "Sample" $ do
    sample `shouldBe` ("", "")

  it "Actual" $ do
    actual `shouldBe` ("", "")
