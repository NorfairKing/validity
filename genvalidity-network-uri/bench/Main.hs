{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.GenValidity.Criterion
import Data.GenValidity.URI ()
import Network.URI

main :: IO ()
main =
  Criterion.defaultMain
    [ bgroup
        "generators"
        [ genValidBench @URIAuth,
          genValidBench @URI
        ],
      bgroup
        "shrinkers"
        [ shrinkValidBench @URIAuth,
          shrinkValidBench @URI
        ]
    ]
