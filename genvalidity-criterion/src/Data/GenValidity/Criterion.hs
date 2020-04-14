{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Benchmarks for generators
module Data.GenValidity.Criterion
  ( genValidityBench,
    genUncheckedBench,
    genValidBench,
    genBenchSizes,
    genBench,
  )
where

import Control.DeepSeq
import Criterion
import Data.GenValidity
import Data.Typeable
import Test.QuickCheck

-- | Benchmarks for both genUnchecked and genValid
genValidityBench ::
  forall a.
  (Typeable a, NFData a, GenUnchecked a, GenValid a) =>
  Benchmark
genValidityBench =
  bgroup (unwords ["GenValidity", nameOf @a]) [genValidBench @a, genUncheckedBench @a]

-- | Benchmarks for both genUnchecked
genUncheckedBench ::
  forall a.
  (Typeable a, NFData a, GenUnchecked a) =>
  Benchmark
genUncheckedBench = genBenchSizes (unwords ["genUnchecked", nameOf @a]) (genUnchecked @a)

-- | Benchmarks for both genValid
genValidBench ::
  forall a.
  (Typeable a, NFData a, GenValid a) =>
  Benchmark
genValidBench = genBenchSizes (unwords ["genValid", nameOf @a]) (genValid @a)

-- | Benchmarks a generator with some default sizes
genBenchSizes :: NFData a => String -> Gen a -> Benchmark
genBenchSizes name gen =
  bgroup name $
    let bi i = genBench ("size " <> show i) (resize i gen)
     in [bi 8, bi 16, bi 32, bi 64]

-- | Benchmarks a generator with a given name
genBench :: NFData a => String -> Gen a -> Benchmark
genBench name gen =
  bench name $ nfIO $ generate gen

nameOf ::
  forall a.
  Typeable a =>
  String
nameOf =
  let s = show $ typeRep (Proxy @a)
   in if ' ' `elem` s
        then "(" ++ s ++ ")"
        else s
