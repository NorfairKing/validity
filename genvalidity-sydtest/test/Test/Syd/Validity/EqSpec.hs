{-# LANGUAGE TypeApplications #-}

-- | Standard 'Spec's for 'Eq' instances.
--
-- You will need @TypeApplications@ to use these.
module Test.Syd.Validity.EqSpec where

import Data.GenValidity
import Test.Syd
import Test.Syd.Validity.Eq

spec :: Spec
spec = do
  eqSpecOnValid @Rational
  eqSpec @Int
  -- eqSpec @Double DOES NOT HOLD because of NaN
  eqSpecOnArbitrary @Int
  eqSpecOnGen ((* 2) <$> genValid @Int) "even" (const [])
