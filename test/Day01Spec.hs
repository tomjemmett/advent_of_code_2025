module Day01Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 1" $ do
  (sample, actual) <- runIO day01

  it "Sample" $ do
    sample `shouldBe` ("3", "6")

  it "Actual" $ do
    actual `shouldBe` ("1036", "6228")
