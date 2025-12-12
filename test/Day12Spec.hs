module Day12Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 12" $ do
  (sample, actual) <- runIO day12

  it "Sample" $ do
    sample `shouldBe` ("2", "")

  it "Actual" $ do
    actual `shouldBe` ("454", "")
