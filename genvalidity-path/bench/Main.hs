{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main
import Test.QuickCheck

import Path

import Data.GenValidity
import Data.GenValidity.Path ()

main :: IO ()
main =
    defaultMain
        [ bench "genValid Path Abs File" . nfIO $
          generate (genValid @(Path Abs File))
        , bench "genValid Path Rel File" . nfIO $
          generate (genValid @(Path Rel File))
        , bench "genValid Path Abs Dir" . nfIO $
          generate (genValid @(Path Abs Dir))
        , bench "genValid Path Rel Dir" . nfIO $
          generate (genValid @(Path Rel Dir))
        , bench "genUnchecked Path Abs File" . nfIO $
          generate (genUnchecked @(Path Abs File))
        , bench "genUnchecked Path Rel File" . nfIO $
          generate (genUnchecked @(Path Rel File))
        , bench "genUnchecked Path Abs Dir" . nfIO $
          generate (genUnchecked @(Path Abs Dir))
        , bench "genUnchecked Path Rel Dir" . nfIO $
          generate (genUnchecked @(Path Rel Dir))
        , bench "genInvalid Path Abs File" . nfIO $
          generate (genInvalid @(Path Abs File))
        , bench "genInvalid Path Rel File" . nfIO $
          generate (genInvalid @(Path Rel File))
        , bench "genInvalid Path Abs Dir" . nfIO $
          generate (genInvalid @(Path Abs Dir))
        , bench "genInvalid Path Rel Dir" . nfIO $
          generate (genInvalid @(Path Rel Dir))
        ]
