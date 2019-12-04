{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Benchmarks for generators
module Data.GenValidity.Criterion
  ( genValidityBench
  , genUncheckedBench
  , genValidBench
  , genBench
  ) where

import Data.GenValidity
import Data.Typeable
import Test.QuickCheck

import Criterion

-- | Benchmarks for both genUnchecked and genValid
genValidityBench ::
     forall a. (Typeable a, GenUnchecked a, GenValid a)
  => Benchmark
genValidityBench =
  bgroup (unwords ["GenValidity", nameOf @a]) [genValidBench @a, genUncheckedBench @a]

-- | Benchmarks for both genUnchecked
genUncheckedBench ::
     forall a. (Typeable a, GenUnchecked a)
  => Benchmark
genUncheckedBench = genBench (unwords ["genUnchecked", nameOf @a]) (genUnchecked @a)

-- | Benchmarks for both genValid
genValidBench ::
     forall a. (Typeable a, GenValid a)
  => Benchmark
genValidBench = genBench (unwords ["genValid", nameOf @a]) (genValid @a)

-- | Benchmarks a generator with a given name
genBench :: String -> Gen a -> Benchmark
genBench name gen = bench name $ whnfIO $ generate (resize 30 gen)

nameOf ::
     forall a. Typeable a
  => String
nameOf = show $ typeRep (Proxy @a)
