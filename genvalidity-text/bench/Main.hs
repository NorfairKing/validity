{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Text as Strict
import Data.Text.Lazy as Lazy

import Criterion.Main as Criterion

import Data.GenValidity
import Test.QuickCheck

import Data.GenValidity.Criterion
import Data.GenValidity.Text

main :: IO ()
main =
  Criterion.defaultMain
    [ bgroup
        "Instances"
        [genBench "Strict.Text" (genValid @Strict.Text), genBench "Lazy.Text" (genValid @Lazy.Text)]
    , bgroup
        "Approaches"
        [ genBench "via list (old version)" $ Strict.pack <$> genValid
        , genBench "genText" genText
        , genBench "genTextBy genValid" $ genTextBy genValid
        , genBench "genTextBy (choose (minBound, maxBound)) (currently in use)" $
          genTextBy (choose (minBound, maxBound))
        ]
    ]
