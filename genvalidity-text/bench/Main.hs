{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.GenValidity
import Data.GenValidity.Criterion
import Data.GenValidity.Text ()
import Data.Text as Strict
import Data.Text.Lazy as Lazy

main :: IO ()
main =
  Criterion.defaultMain
    [ bgroup
        "generators"
        [ genBench "Strict.Text" (genValid @Strict.Text),
          genBench "Lazy.Text" (genValid @Lazy.Text)
        ],
      bgroup
        "shrinkers"
        [ shrinkBench "Strict.Text" (shrinkValid @Strict.Text),
          shrinkBench "Lazy.Text" (shrinkValid @Lazy.Text)
        ]
    ]
