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
    genBenchSized,
  )
where

import Control.DeepSeq
import Criterion
import Data.GenValidity
import Data.Typeable
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

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
    let bi i = genBenchSized ("size " <> show i) i gen
     in [bi 15, bi 30]

-- | Benchmarks a generator with a given name with the default size: 30
genBench :: NFData a => String -> Gen a -> Benchmark
genBench name = genBenchSized name 30

-- | Benchmarks a generator with a given name and size
genBenchSized :: NFData a => String -> Int -> Gen a -> Benchmark
genBenchSized name size (MkGen genFunc) =
  bench name $ nf (\seed -> genFunc seed size) (mkQCGen 42)

nameOf ::
  forall a.
  Typeable a =>
  String
nameOf =
  let s = show $ typeRep (Proxy @a)
   in if ' ' `elem` s
        then "(" ++ s ++ ")"
        else s
