{-# LANGUAGE TypeApplications #-}

module Test.Validity.FunctorSpec where

import Test.Hspec

import Data.GenValidity
import Test.Validity.Functor
import Test.Validity.TestUtils

spec :: Spec
spec = do
    functorSpec @[]
    functorSpec @Maybe
    failsBecause "Fcks does not satisfy any Functor laws" $ functorSpec @Fcks
    functorSpec @(Either Int)
    functorSpec @((,) Int)
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
        "scalings"
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
        "appends"

data Fcks a =
    Fcks Int
    deriving (Show, Eq)

instance GenUnchecked (Fcks a) where
    genUnchecked = Fcks <$> genUnchecked

instance Functor Fcks where
    fmap _ (Fcks i) = Fcks $ i * 2
