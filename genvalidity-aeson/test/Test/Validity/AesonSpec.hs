{-# LANGUAGE TypeApplications #-}

module Test.Validity.AesonSpec where

import Control.DeepSeq
import Control.Exception (evaluate)
import Data.Aeson (Value, encode)
import Data.GenValidity.Aeson ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @Value
  describe "genValid :: Gen Value" $
    it "produces deepseqable values" $
      forAllValid $ \v ->
        evaluate (deepseq (encode (v :: Value)) ()) `shouldReturn` ()
