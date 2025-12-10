module Day06Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 6" $ do
  (sample, actual) <- runIO day06

  it "Sample" $ do
    sample `shouldBe` ("4277556", "3263827")

  it "Actual" $ do
    actual `shouldBe` ("6635273135233", "12542543681221")
