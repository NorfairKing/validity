{-# LANGUAGE TypeApplications #-}

-- | Standard 'Spec's for 'Show' and 'Read' instances.
--
-- You will need @TypeApplications@ to use these.
module Test.Syd.Validity.ShowSpec where

import Data.GenValidity
import Test.Syd
import Test.Syd.Validity.Show

spec :: Spec
spec = do
  showReadSpec @Rational
  showReadSpec @Int
  showReadSpecOnArbitrary @Rational
  showReadSpecOnGen ((* 2) <$> genValid @Int) "even" (const [])
