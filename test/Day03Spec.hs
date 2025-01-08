module Day03Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 3" $ do
  (sample, actual) <- runIO day03

  it "Sample" $ do
    sample `shouldBe` ("", "")

  it "Actual" $ do
    actual `shouldBe` ("", "")
