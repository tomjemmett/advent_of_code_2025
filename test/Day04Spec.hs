module Day04Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 4" $ do
  (sample, actual) <- runIO day04

  it "Sample" $ do
    sample `shouldBe` ("13", "43")

  it "Actual" $ do
    actual `shouldBe` ("1457", "8310")
