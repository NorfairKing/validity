{-# LANGUAGE TypeApplications #-}

module Test.Validity.FunctorSpec where

import Test.Hspec

import Data.GenValidity
import Test.Validity.Functor

spec :: Spec
spec = do
    functorSpec @[]
    functorSpec @Maybe
    functorSpecOnValid @[]
    functorSpecOnValid @Maybe
    functorSpecOnArbitrary @[]
    functorSpecOnArbitrary @Maybe
    functorSpecOnGens
        @[]
        @Int
        (pure 4)
        "four"
        (genListOf $ pure 5)
        "list of fives"
        ((+) <$> genValid)
        "increments"
        ((*) <$> genValid)
        "factorisations"

    functorSpecOnGens
        @Maybe
        @String
        (pure "ABC")
        "ABC"
        (Just <$> pure "ABC")
        "Just an ABC"
        ((++) <$> genValid)
        "prepends"
        ((flip (++)) <$> genValid)
        "flipped concatenations"
