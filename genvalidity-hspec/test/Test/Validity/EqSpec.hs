{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Test.Validity.EqSpec where

import Data.GenValidity
import GHC.Generics (Generic)
import Test.Hspec
import Test.Validity.Eq
import Test.Validity.Utils

spec :: Spec
spec = do
  eqSpec @Rational
  eqSpec @Int
  -- eqSpec @Double DOES NOT HOLD because of NaN
  eqSpecOnArbitrary @Int
  eqSpecOnGen ((* 2) <$> genValid @Int) "even" (const [])
  failsBecause "(/=) and (==) don't have opposite semantics" $
    eqSpec @EqFuncMismatch

newtype EqFuncMismatch
  = EqFuncMismatch ()
  deriving (Show, Generic)

instance Validity EqFuncMismatch

instance Eq EqFuncMismatch where
  (==) _ _ = True
  (/=) _ _ = True

instance GenValid EqFuncMismatch where
  genValid = EqFuncMismatch <$> genValid
  shrinkValid _ = []
