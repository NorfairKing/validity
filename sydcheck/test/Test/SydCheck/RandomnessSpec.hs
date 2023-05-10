module Test.SydCheck.RandomnessSpec (spec) where

import Test.Syd
import Test.SydCheck.Randomness

spec :: Spec
spec = do
  describe "computeSplit" $
    it "shrinks to 0" $
      computeSplit 30 0 `shouldBe` 0
