module Data.GenValidity.UUIDSpec
  ( spec,
  )
where

import Data.GenValidity
import Data.GenValidity.UUID ()
import Data.UUID
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
  describe "genValid" $
    it "generates valid UUId's" $ forAll (genValid :: Gen UUID) isValid
