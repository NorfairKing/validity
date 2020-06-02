{-# LANGUAGE TypeApplications #-}

module Data.GenValidity.Containers.SeqSpec where

import Data.GenValidity.Sequence ()
import Data.Sequence (Seq)
import Test.Hspec
import Test.Validity.GenValidity

spec :: Spec
spec = do
  genValidSpec @(Seq Int)
  genValiditySpec @(Seq Rational)
