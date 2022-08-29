{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Benchmarks for generators
module Data.GenValidity.Criterion
  ( genValidBench,
    genBench,
    genBenchSized,
    shrinkValidBench,
    shrinkBench,
    shrinkBenchN,
    shrinkBenchVector,
  )
where

import Control.DeepSeq
import Criterion
import Data.GenValidity
import Data.Typeable
import Data.Vector (Vector)
import qualified Data.Vector as V
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

-- | Benchmarks for both genValid
genValidBench ::
  forall a.
  (Typeable a, NFData a, GenValid a) =>
  Benchmark
genValidBench = genBench (unwords ["genValid", nameOf @a]) (genValid @a)

-- | Benchmarks a generator with some default sizes
genBench :: NFData a => String -> Gen a -> Benchmark
genBench name gen =
  bgroup name $
    let bi i = genBenchSized ("size " <> show i) i gen
     in [bi 15, bi 30]

-- | Benchmarks a generator with a given name and size
genBenchSized :: NFData a => String -> Int -> Gen a -> Benchmark
genBenchSized name size gen =
  let MkGen genFunc = V.replicateM 100 gen
   in bench name $ nf (\seed -> genFunc seed size) (mkQCGen 42)

-- | Benchmark for the time it takes to shrink to the first ten shrunk versions using 'shrinkValid' and a vector of 100 deterministically generated values.
shrinkValidBench ::
  forall a.
  (Typeable a, NFData a, GenValid a) =>
  Benchmark
shrinkValidBench =
  shrinkBench
    (unwords ["shrinkValid", nameOf @a])
    (shrinkValid @a)

-- | Benchmark for the time it takes to shrink to the first ten shrunk versions using a given shrinking function and a vector of 100 deterministically generated values.
shrinkBench :: (GenValid a, NFData a) => String -> (a -> [a]) -> Benchmark
shrinkBench = shrinkBenchN 100

-- | Benchmark for the time it takes to shrink to the first ten shrunk versions using a given shrinking function and a vector of N deterministically generated values.
shrinkBenchN :: forall a. (GenValid a, NFData a) => Int -> String -> (a -> [a]) -> Benchmark
shrinkBenchN n name shrinker =
  withArgs n $ \args -> shrinkBenchVector args name shrinker

-- | Benchmark for the time it takes to shrink to the first ten shrunk versions using a given shrinking function and a given vector of values
shrinkBenchVector :: forall a. NFData a => Vector a -> String -> (a -> [a]) -> Benchmark
shrinkBenchVector args name shrinker =
  bench
    name
    (nf (V.map (take 10 . shrinker)) (args :: Vector a))

withArgs :: (NFData arg, GenValid arg) => Int -> (Vector arg -> Benchmark) -> Benchmark
withArgs n = env (pure (generateDeterministically $ V.replicateM n genValid))

generateDeterministically :: Gen a -> a
generateDeterministically (MkGen f) = f seed size
  where
    seed = mkQCGen 42
    size = 30

nameOf ::
  forall a.
  Typeable a =>
  String
nameOf =
  let s = show $ typeRep (Proxy @a)
   in if ' ' `elem` s
        then "(" ++ s ++ ")"
        else s
