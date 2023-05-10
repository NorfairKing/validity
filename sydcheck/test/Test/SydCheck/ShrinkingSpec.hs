{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.SydCheck.ShrinkingSpec (spec) where

import qualified Data.Vector.Unboxed as UV
import Test.Syd
import Test.SydCheck.Randomness
import Test.SydCheck.Shrinking

spec :: Spec
spec = do
  describe "shrinkRandomness" $ do
    it "does not shrink an empty vector" $
      shrinkRandomness emptyRandomness
        `shouldBe` []
    it "always tries to shrink to a smaller vector from the beginning" $
      let shrinks = shrinkRandomness $ Randomness $ UV.fromList [1, 2, 3]
       in shrinks `shouldSatisfy` (Randomness (UV.fromList [2, 3]) `elem`)
    it "always tries to shrink to a smaller vector from the end" $
      let shrinks = shrinkRandomness $ Randomness $ UV.fromList [1, 2, 3]
       in shrinks `shouldSatisfy` (Randomness (UV.fromList [1, 2]) `elem`)
