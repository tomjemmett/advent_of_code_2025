module Day07Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 7" $ do
  (sample, actual) <- runIO day07

  it "Sample" $ do
    sample `shouldBe` ("21", "40")

  it "Actual" $ do
    actual `shouldBe` ("1562", "24292631346665")
