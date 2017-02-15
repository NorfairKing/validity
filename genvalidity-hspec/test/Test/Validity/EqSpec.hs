{-# LANGUAGE TypeApplications #-}

module Test.Validity.EqSpec where

import Test.Hspec

import Data.GenValidity
import Test.Validity.Eq
import Test.Validity.TestUtils

spec :: Spec
spec = do
    eqSpecOnValid @Double
    eqSpec @Int
    failsBecause "reflexivity does not hold for NaN" $ eqSpecOnInvalid @Double
    eqSpecOnArbitrary @Int
    eqSpecOnGen ((* 2) <$> genValid @Int) "even"
    failsBecause "(/=) and (==) don't have opposite semantics" $
        eqSpec @EqFuncMismatch

newtype EqFuncMismatch =
    EqFuncMismatch ()
    deriving (Show)

instance Eq EqFuncMismatch where
    (==) _ _ = True
    (/=) _ _ = True

instance GenUnchecked EqFuncMismatch where
    genUnchecked = EqFuncMismatch <$> genUnchecked
