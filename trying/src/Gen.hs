{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Gen where

import Data.Validity
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as UV
import Data.Word
import Debug.Trace
import System.Random.SplitMix as SM

-- Integrated shrinking AND size handling.
--
-- The length of the vector is the size paremeter, in some sense.
-- That's how much randomness the generator is allowed to use.
data Gen a where
  Gen :: (Vector Word64 -> a) -> Gen a
  GenPure :: a -> Gen a
  GenFMap :: (a -> b) -> Gen a -> Gen b
  GenAp :: Gen (a -> b) -> Gen a -> Gen b

instance Functor Gen where
  fmap = GenFMap

instance Applicative Gen where
  pure = GenPure
  (<*>) = GenAp

runGen :: Gen a -> Vector Word64 -> a
runGen = flip go
  where
    go :: Vector Word64 -> Gen a -> a
    go ws = \case
      Gen fun -> fun ws
      GenPure a -> a
      GenFMap f g' -> f $ go ws g'
      GenAp ff fa ->
        let len = UV.length ws
            (leftWs, rightWs) = case len of
              0 -> (UV.empty, UV.empty)
              1 -> (UV.empty, UV.empty)
              n ->
                let leftSize = computeSplit (pred len) (UV.head ws)
                    restRandomness = UV.tail ws
                 in (UV.take leftSize ws, UV.drop leftSize ws)
         in (go leftWs ff) (go rightWs fa)

-- | Compute an arbitrarily split value
--
-- Input: number n
-- Output: number between 0 and n (inclusive), distributed uniformly
--
-- When randomWord is 0, the split is shrunk as much as possible.
-- In that case we want to return the most shrunk split, so (0, n)
computeSplit :: Int -> Word64 -> Int
computeSplit totalSize randomWord =
  let left = randomWord `rem` (fromIntegral (totalSize + 1))
   in fromIntegral left

-- | Compute an arbitrary partition
--
-- Input: number n
-- Output: list ls such that sum ls equals 'n'
--
-- When randomWord is 0, the split is shrunk as much as possible.
-- In that case we want to return the most shrunk partition,
-- so either [] (for size 0), or [size] for nonzero size.
computePartition :: Int -> Word64 -> [Int]
computePartition totalSize randomWord =
  undefined

-- Laws:
-- 1: Every generated value must be valid
-- 2: With enough randomness, every valid value must be generated eventually.
class Validity a => GenValid a where
  genValid :: Gen a

instance GenValid Bool where
  genValid = genBool False

genBool :: Bool -> Gen Bool
genBool minimal = Gen $ \v ->
  if UV.null v
    then minimal
    else even (UV.head v)

instance (GenValid a, GenValid b) => GenValid (a, b) where
  genValid = (,) <$> genValid <*> genValid

instance GenValid a => GenValid [a] where
  genValid = genListOf genValid

instance GenValid Word8 where
  genValid = Gen $ \v ->
    if UV.null v
      then 0
      else fromIntegral (UV.head v `rem` (fromIntegral (maxBound :: Word8)))

instance GenValid Word64 where
  genValid = Gen $ \v ->
    if UV.null v
      then 0
      else UV.head v

genListOf :: Gen a -> Gen [a]
genListOf = undefined

computeRandomness :: Int -> Word64 -> Vector Word64
computeRandomness size seed =
  UV.unfoldrExactN size SM.nextWord64 smGen
  where
    smGen = mkSMGen seed

-- Pretend every shrunk version still fails.
computeAllShrinks :: Vector Word64 -> [Vector Word64]
computeAllShrinks v =
  if UV.null v
    then []
    else
      let vs = shrinkRandomness v
       in if null vs
            then []
            else concatMap computeAllShrinks vs ++ vs

-- Compute the next shrinking steps.
--
-- Laws:
--
-- 1. Must never shrink to itself.
shrinkRandomness :: Vector Word64 -> [Vector Word64]
shrinkRandomness v =
  if UV.null v
    then []
    else [UV.init v, UV.tail v]

-- Compute the next shrinking step for this value.
shrinkSingleWord64 :: Word64 -> [Word64]
shrinkSingleWord64 w =
  let log = logBase 2 (fromIntegral w)
   in []
