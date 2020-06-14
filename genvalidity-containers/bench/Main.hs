{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.GenValidity
import Data.GenValidity.Containers
import Data.GenValidity.Criterion
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Tree (Forest, Tree)
import Test.QuickCheck

main :: IO ()
main =
  Criterion.defaultMain
    [ genValidBench @(Set Int),
      genValidBench @(Seq Int),
      genValidBench @(Tree Int),
      genValidBench @(Forest Int),
      genValidBench @(Map Int Int),
      genBenchSizes "genSeqOf" $ genSeqOf (genValid :: Gen Int),
      genBenchSizes "genSetOf" $ genSetOf (genValid :: Gen Int),
      genBenchSizes "genMapOf" $ genMapOf (genValid :: Gen (Int, Int))
    ]
