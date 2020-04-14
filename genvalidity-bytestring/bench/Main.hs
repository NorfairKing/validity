{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import Data.GenValidity
import Data.GenValidity.ByteString
import Data.GenValidity.Criterion
import Test.QuickCheck

main :: IO ()
main =
  Criterion.defaultMain
    [ bgroup
        "Instances"
        [ genValidBench @Strict.ByteString,
          genValidBench @Lazy.ByteString
        ],
      bgroup
        "Approaches"
        [ genBench "SB.pack <$> genValid" (Strict.pack <$> genValid),
          genBench "LB.pack <$> genValid" (Lazy.pack <$> genValid)
        ]
    ]
