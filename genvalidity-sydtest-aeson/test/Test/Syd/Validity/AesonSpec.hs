{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Syd.Validity.AesonSpec where

import Data.Aeson
import Data.GenValidity
import Data.GenValidity.Aeson ()
import Data.GenValidity.Text ()
import Data.Text (Text)
import GHC.Generics
import Test.Syd
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  jsonSpecOnGen (genListOf $ pure 'a') "sequence of 'a's" (const [])
  -- jsonSpec @Double DOES NOT HOLD
  jsonSpecOnValid @Rational
  jsonSpec @Int
  jsonSpecOnArbitrary @Int
  jsonSpecOnValid @ForShow
  jsonSpecOnValid @Value

-- shrinkValidSpec @Value

newtype ForShow
  = ForShow Text
  deriving (Show, Eq, Generic)

instance Validity ForShow

instance GenValid ForShow where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance FromJSON ForShow

instance ToJSON ForShow
