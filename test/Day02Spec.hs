module Day02Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 2" $ do
  (sample, actual) <- runIO day02

  it "Sample" $ do
    sample `shouldBe` ("1227775554", "4174379265")

  it "Actual" $ do
    actual `shouldBe` ("18893502033", "26202168557")
