{-# LANGUAGE TypeApplications #-}

module Test.Validity.FunctorSpec where

import Test.Hspec

import Data.GenValidity
import Test.Validity.Functor

spec :: Spec
spec = do
    functorSpecOnGens
        @[]
        @Int
        (pure 4)
        "four"
        (genListOf $ pure 5)
        "list of fives"
        ((+) <$> genValid)
        "additions"
        ((*) <$> genValid)
        "multiplications"
    functorSpecOnGens
        @Maybe
        @String
        (pure "ABC")
        "ABC"
        (Just <$> pure "ABC")
        "Just an ABC"
        ((++) <$> genValid)
        "concatenations"
        ((flip (++)) <$> genValid)
        "flipped concatenations"
