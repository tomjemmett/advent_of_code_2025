module Day08Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 8" $ do
  (sample, actual) <- runIO day08

  it "Sample" $ do
    sample `shouldBe` ("", "")

  it "Actual" $ do
    actual `shouldBe` ("", "")
