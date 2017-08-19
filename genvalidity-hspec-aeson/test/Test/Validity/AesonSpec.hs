{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Validity.AesonSpec where

import Test.Hspec

import Data.Aeson
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.Text (Text)
import GHC.Generics
import Test.Validity.Aeson

spec :: Spec
spec = do
    jsonSpecOnGen (genListOf $ pure 'a') "sequence of 'a's"
    jsonSpecOnValid @Double
    jsonSpec @Int
    jsonSpecOnArbitrary @Int
    jsonSpecOnValid @ForShow

newtype ForShow =
    ForShow Text
    deriving (Show, Eq, Generic)

instance Validity ForShow

instance GenUnchecked ForShow

instance GenValid ForShow

instance FromJSON ForShow

instance ToJSON ForShow
-- >>> decode (Data.Aeson.encode (ForShow "\248")) :: Maybe ForShow
-- Just (ForShow "\248")
