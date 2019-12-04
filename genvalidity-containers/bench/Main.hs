{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Tree (Forest, Tree)

import Criterion.Main as Criterion


import Data.GenValidity.Containers ()
import Data.GenValidity.Criterion

main :: IO ()
main =
  Criterion.defaultMain
    [ genValidBench @(Set Int)
    , genValidBench @(Seq Int)
    , genValidBench @(Tree Int)
    , genValidBench @(Forest Int)
    , genValidBench @(Map Int Int)
    ]
