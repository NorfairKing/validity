module SydCheck.RandomnessSpec (spec) where

import SydCheck.Randomness
import Test.Syd

spec :: Spec
spec = do
  describe "computeSplit" $
    it "shrinks to 0" $
      computeSplit 30 0 `shouldBe` 0
