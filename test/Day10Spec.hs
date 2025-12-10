module Day10Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 10" $ do
  (sample, actual) <- runIO day10

  it "Sample" $ do
    sample `shouldBe` ("7", "33")

  it "Actual" $ do
    actual `shouldBe` ("432", "18011")
