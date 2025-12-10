module Day08Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 8" $ do
  (sample, actual) <- runIO day08

  it "Sample" $ do
    sample `shouldBe` ("40", "25272")

  it "Actual" $ do
    actual `shouldBe` ("115885", "274150525")
