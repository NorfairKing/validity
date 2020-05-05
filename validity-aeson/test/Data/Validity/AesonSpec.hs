{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Validity.AesonSpec where

import Data.Aeson
import Data.Validity
import Data.Validity.Aeson
import GHC.Generics (Generic)
import Test.Hspec

data MyRatio = MyRatio Word Word
  deriving (Show, Eq, Generic)

instance Validity MyRatio where
  validate mr@(MyRatio _ d) = mconcat [genericValidate mr, declare "The denominator is not zero" $ d /= 0]

instance FromJSON MyRatio where
  parseJSON = parseJSONValidWith . withObject "MyRatio" $ \o ->
    MyRatio <$> o .: "numerator" <*> o .: "denominator"

spec :: Spec
spec =
  describe "parseJSONValid" $ do
    it "does not allow this invalid ratio to be parsed" $
      case fromJSON (object ["numerator" .= (1 :: Word), "denominator" .= (0 :: Word)]) of
        Error _ -> pure ()
        Success (MyRatio _ _) -> expectationFailure "Should have failed."
