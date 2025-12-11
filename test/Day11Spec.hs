module Day11Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 11" $ do
  (sample, actual) <- runIO day11

  it "Sample" $ do
    sample `shouldBe` ("5", "2")

  it "Actual" $ do
    actual `shouldBe` ("428", "331468292364745")
