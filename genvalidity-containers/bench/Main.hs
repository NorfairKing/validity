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
    [ bgroup
        "generators"
        [ genValidBench @(Set Int),
          genValidBench @(Seq Int),
          genValidBench @(Tree Int),
          genValidBench @(Forest Int),
          genValidBench @(Map Int Int),
          genBench "genSeqOf" $ genSeqOf (genValid :: Gen Int),
          genBench "genSetOf" $ genSetOf (genValid :: Gen Int),
          genBench "genMapOf" $ genMapOf (genValid :: Gen (Int, Int)),
          genBench "genTreeOf" $ genTreeOf (genValid :: Gen (Int, Int))
        ],
      bgroup
        "shrinkers"
        [ shrinkValidBench @(Set Int),
          shrinkValidBench @(Seq Int),
          shrinkValidBench @(Tree Int),
          shrinkValidBench @(Forest Int),
          shrinkValidBench @(Map Int Int),
          shrinkBench "shrinkSeqOf" $ shrinkSeqOf (shrinkValid :: Int -> [Int]),
          shrinkBench "shrinkSetOf" $ shrinkSetOf (shrinkValid :: Int -> [Int]),
          shrinkBench "shrinkMapOf" $ shrinkMapOf (shrinkValid :: (Int, Int) -> [(Int, Int)]),
          shrinkBench "shrinkTreeOf" $ shrinkTreeOf (shrinkValid :: Int -> [Int])
        ]
    ]
