{-# LANGUAGE TypeApplications #-}

-- | Standard 'Spec's for 'Eq' instances.
--
-- You will need @TypeApplications@ to use these.
module Test.Validity.EqSpec where

import Data.GenValidity
import Test.Hspec
import Test.Validity.Eq
import Test.Validity.Utils

spec :: Spec
spec = do
  eqSpecOnValid @Rational
  eqSpec @Int
  -- eqSpec @Double DOES NOT HOLD because of NaN
  eqSpecOnArbitrary @Int
  eqSpecOnGen ((* 2) <$> genValid @Int) "even" (const [])
  failsBecause "(/=) and (==) don't have opposite semantics" $
    eqSpec @EqFuncMismatch

newtype EqFuncMismatch
  = EqFuncMismatch ()
  deriving (Show)

instance Eq EqFuncMismatch where
  (==) _ _ = True
  (/=) _ _ = True

instance GenUnchecked EqFuncMismatch where
  genUnchecked = EqFuncMismatch <$> genUnchecked
  shrinkUnchecked _ = []
