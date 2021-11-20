{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Test.Validity.ShowSpec where

import Data.GenValidity
import GHC.Generics (Generic)
import Test.Hspec
import Test.Validity.Show
import Test.Validity.Utils

spec :: Spec
spec = do
  showReadSpec @Rational
  showReadSpec @Int
  showReadSpecOnArbitrary @Rational
  showReadSpecOnGen ((* 2) <$> genValid @Int) "even" (const [])
  failsBecause "show and read don't have the correct semantics" $
    showReadSpec @ShowFuncMismatch

data ShowFuncMismatch
  = ShowFuncMismatch
  deriving (Eq, Read, Generic)

instance Validity ShowFuncMismatch

instance Show ShowFuncMismatch where
  show ShowFuncMismatch = "wrong"

instance GenValid ShowFuncMismatch where
  genValid = pure ShowFuncMismatch
  shrinkValid _ = []
