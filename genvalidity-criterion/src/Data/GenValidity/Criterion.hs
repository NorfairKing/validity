{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.GenValidity.Criterion where

import Data.GenValidity
import Data.Typeable
import Test.QuickCheck

import Criterion

genValidityBench ::
     forall a. (Typeable a, GenUnchecked a,GenValid a)
  => Benchmark
genValidityBench =
  bgroup (unwords ["GenValidity", nameOf @a]) [genValidBench @a, genUncheckedBench @a]

genUncheckedBench ::
     forall a. (Typeable a, GenUnchecked a)
  => Benchmark
genUncheckedBench = genBench "genUnchecked" (genUnchecked @a)

genValidBench ::
     forall a. (Typeable a, GenValid a)
  => Benchmark
genValidBench = genBench "genValid" (genValid @a)

genBench ::
     forall a. Typeable a
  => String
  -> Gen a
  -> Benchmark
genBench s gen = bench (unwords [s, nameOf @a]) $ whnfIO $ generate (resize 30 gen)

nameOf ::
     forall a. Typeable a
  => String
nameOf = show $ typeRep (Proxy @a)
