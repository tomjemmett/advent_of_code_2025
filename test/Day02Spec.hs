module Day02Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 2" $ do
  (sample, actual) <- runIO day02

  it "Sample" $ do
    sample `shouldBe` ("", "")

  it "Actual" $ do
    actual `shouldBe` ("", "")
