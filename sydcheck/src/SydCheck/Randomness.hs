{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SydCheck.Randomness where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as UV
import Data.Word
import System.Random.SplitMix as SM

-- | Size parameter
--
-- The size parameter represents how much randomness a generator can/is allowed to take.
-- Some generators generate larger structures if more randomness is presented.
newtype Size = Size {unSize :: Int}
  deriving stock (Show, Read, Eq, Ord)
  deriving newtype (Num, Enum, Bounded)

-- TODO: Newtype?
type Seed = RandomWord

-- TODO: Newtype?
type RandomWord = Word64

-- TODO: Newtype?
type Randomness = Vector Word64

-- Integrated shrinking AND size handling.

-- | Compute an arbitrarily split value
--
-- Input: number n
-- Output: number between 0 and n (inclusive), distributed uniformly
--
-- When randomWord is 0, the split is shrunk as much as possible.
-- In that case we want to return the most shrunk split, so (0, n)
computeSplit :: Int -> RandomWord -> Int
computeSplit totalSize randomWord =
  let left = randomWord `rem` (fromIntegral (totalSize + 1))
   in fromIntegral left

-- | Compute a randomness vector based on a size and seed
computeRandomness :: Size -> Seed -> Randomness
computeRandomness size seed = computeRandomnessWithSMGen size (mkSMGen seed)

-- | Compute a randomness vector based on a size and splitmix generator
computeRandomnessWithSMGen :: Size -> SMGen -> Randomness
computeRandomnessWithSMGen (Size size) = UV.unfoldrExactN size SM.nextWord64

sizeRandomness :: Randomness -> Size
sizeRandomness = Size . UV.length

takeRandomness :: Size -> Randomness -> Randomness
takeRandomness = UV.take . unSize

dropRandomness :: Size -> Randomness -> Randomness
dropRandomness = UV.drop . unSize

splitRandomnessAt :: Size -> Randomness -> (Randomness, Randomness)
splitRandomnessAt = UV.splitAt . unSize

computeSplitRandomness :: Randomness -> (Randomness, Randomness)
computeSplitRandomness ws =
  let len = UV.length ws
   in case len of
        0 -> (UV.empty, UV.empty)
        1 -> (UV.empty, UV.empty)
        _ ->
          let leftSize = computeSplit (pred len) (UV.head ws)
              restRandomness = UV.tail ws
           in UV.splitAt leftSize restRandomness
