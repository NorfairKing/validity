{-# LANGUAGE TypeApplications #-}

module Test.Syd.Validity.MonadSpec where

import Data.GenValidity
import Test.Syd
import Test.Syd.Validity.Monad

{-# ANN module "HLint: ignore Use :" #-}

spec :: Spec
spec = do
  monadSpec @[]
  monadSpec @Maybe
  monadSpec @(Either Int)
  monadSpecOnArbitrary @[]
  monadSpecOnArbitrary @Maybe
  monadSpecOnGens
    @[]
    @Int
    (pure 4)
    "four"
    (genListOf $ pure 5)
    "list of fives"
    (genListOf $ pure 6)
    "list of sixes"
    ((*) <$> genValid)
    "factorisations"
    (pure $ \a -> [a])
    "singletonisation"
    (pure $ \a -> [a])
    "singletonisation"
    (pure $ pure (+ 1))
    "increment in list"
  monadSpecOnGens
    @Maybe
    @String
    (pure "ABC")
    "ABC"
    (Just <$> pure "ABC")
    "Just an ABC"
    (Just <$> pure "CDE")
    "Just an ABC"
    (flip (++) <$> genValid)
    "appends"
    (pure $ \a -> Just a)
    "justisation"
    (pure $ \a -> Just a)
    "justisation"
    (pure $ pure (++ "a"))
    "append 'a' in Just"
