module Day06Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 6" $ do
  (sample, actual) <- runIO day06

  it "Sample" $ do
    sample `shouldBe` ("", "")

  it "Actual" $ do
    actual `shouldBe` ("", "")
