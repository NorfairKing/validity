{-# LANGUAGE TypeApplications #-}

module Test.Validity.VectorSpec where

import Test.Hspec

import Data.GenValidity.Vector ()
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Test.Validity.GenValidity

spec :: Spec
spec = do
    genValidSpec @(V.Vector Int)
    genValiditySpec @(V.Vector Rational)
    genValidSpec @(SV.Vector Int)
