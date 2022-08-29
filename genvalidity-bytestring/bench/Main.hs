{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import Data.GenValidity.ByteString ()
import Data.GenValidity.Criterion

main :: IO ()
main =
  Criterion.defaultMain
    [ bgroup
        "generators"
        [ genValidBench @Strict.ByteString,
          genValidBench @Lazy.ByteString
        ],
      bgroup
        "shrinkers"
        [ shrinkValidBench @Strict.ByteString,
          shrinkValidBench @Lazy.ByteString
        ]
    ]
