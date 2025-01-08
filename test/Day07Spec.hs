module Day07Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 7" $ do
  (sample, actual) <- runIO day07

  it "Sample" $ do
    sample `shouldBe` ("", "")

  it "Actual" $ do
    actual `shouldBe` ("", "")
