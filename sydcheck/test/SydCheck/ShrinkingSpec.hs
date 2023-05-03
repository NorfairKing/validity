{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SydCheck.ShrinkingSpec (spec) where

import qualified Data.Vector.Unboxed as UV
import SydCheck.Shrinking
import Test.Syd

spec :: Spec
spec = do
  describe "shrinkRandomness" $ do
    it "does not shrink an empty vector" $
      shrinkRandomness UV.empty
        `shouldBe` []
    it "always tries to shrink to a smaller vector from the beginning" $
      let shrinks = shrinkRandomness $ UV.fromList [1, 2, 3]
       in shrinks `shouldSatisfy` (UV.fromList [2, 3] `elem`)
    it "always tries to shrink to a smaller vector from the end" $
      let shrinks = shrinkRandomness $ UV.fromList [1, 2, 3]
       in shrinks `shouldSatisfy` (UV.fromList [1, 2] `elem`)
