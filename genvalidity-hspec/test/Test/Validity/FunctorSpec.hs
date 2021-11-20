{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Test.Validity.FunctorSpec where

import Data.GenValidity
import GHC.Generics (Generic)
import Test.Hspec
import Test.Validity.Functor
import Test.Validity.Utils

spec :: Spec
spec = do
  functorSpec @[]
  functorSpec @Maybe
  failsBecause "Fcks does not satisfy any Functor laws" $ functorSpec @Fcks
  functorSpec @(Either Int)
  functorSpec @((,) Int)
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
    (flip (++) <$> genValid)
    "appends"

newtype Fcks a
  = Fcks Int
  deriving (Show, Eq, Generic)

instance Validity (Fcks a)

instance GenValid (Fcks a) where
  genValid = Fcks <$> genValid
  shrinkValid (Fcks i) = Fcks <$> shrinkValid i

instance Functor Fcks where
  fmap _ (Fcks i) = Fcks $ i * 2
