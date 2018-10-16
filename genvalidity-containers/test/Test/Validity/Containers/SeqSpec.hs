{-# LANGUAGE TypeApplications #-}

module Test.Validity.Containers.SeqSpec where

import Test.Hspec

import Data.GenValidity.Sequence ()
import Data.Sequence (Seq)
import Test.Validity.GenValidity

spec :: Spec
spec = do
    genValidSpec @(Seq Int)
    genValiditySpec @(Seq Rational)
