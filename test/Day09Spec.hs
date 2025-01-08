module Day09Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 9" $ do
  (sample, actual) <- runIO day09

  it "Sample" $ do
    sample `shouldBe` ("", "")

  it "Actual" $ do
    actual `shouldBe` ("", "")
