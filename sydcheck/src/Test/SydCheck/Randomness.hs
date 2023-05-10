{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.SydCheck.Randomness where

import Data.Bits (shiftR)
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

-- | A fixed amount of randomness.
--
-- This structure supports efficient splitting.
newtype Randomness = Randomness {unRandomness :: Vector RandomWord}
  deriving stock (Show, Read, Eq, Ord)

-- Integrated shrinking AND size handling.

-- | Compute a randomness vector based on a size and seed
computeRandomness :: Size -> Seed -> Randomness
computeRandomness size seed = computeRandomnessWithSMGen size (mkSMGen seed)

-- | Compute a randomness vector based on a size and splitmix generator
computeRandomnessWithSMGen :: Size -> SMGen -> Randomness
computeRandomnessWithSMGen (Size size) = Randomness . UV.unfoldrExactN size SM.nextWord64

sizeRandomness :: Randomness -> Size
sizeRandomness = Size . UV.length . unRandomness

{-# INLINE emptyRandomness #-}
emptyRandomness :: Randomness
emptyRandomness = Randomness UV.empty

{-# INLINE nullRandomness #-}
nullRandomness :: Randomness -> Bool
nullRandomness = UV.null . unRandomness

{-# INLINE headRandomness #-}
headRandomness :: Randomness -> Maybe RandomWord
headRandomness (Randomness uv) =
  if UV.length uv == 0
    then Nothing
    else Just (uv UV.! 0)

{-# INLINE takeRandomness #-}
takeRandomness :: Size -> Randomness -> Randomness
takeRandomness (Size s) = Randomness . UV.take s . unRandomness

{-# INLINE dropRandomness #-}
dropRandomness :: Size -> Randomness -> Randomness
dropRandomness (Size s) = Randomness . UV.drop s . unRandomness

{-# INLINE splitRandomnessAt #-}
splitRandomnessAt :: Size -> Randomness -> (Randomness, Randomness)
splitRandomnessAt (Size s) (Randomness ws) =
  let (ws1, ws2) = UV.splitAt s ws
   in (Randomness ws1, Randomness ws2)

computeSplitRandomness :: Randomness -> (Randomness, Randomness)
computeSplitRandomness ws =
  let Size len = sizeRandomness ws
   in case len of
        0 -> (emptyRandomness, emptyRandomness)
        1 ->
          -- No point trying to split if there's only one word
          (emptyRandomness, ws)
        _ -> case headRandomness ws of
          Nothing -> (emptyRandomness, emptyRandomness)
          Just rw ->
            let leftSize = computeSplit (Size (pred len)) rw
                restRandomness = dropRandomness 1 ws
             in splitRandomnessAt leftSize restRandomness

-- | Compute an arbitrarily split value
--
-- Input: number n
-- Output: number between 0 and n (inclusive), distributed uniformly
--
-- When randomWord is 0, the split is shrunk as much as possible.
-- In that case we want to return the most shrunk split, so (0, n)
computeSplit :: Size -> RandomWord -> Size
computeSplit (Size totalSize) randomWord =
  Size $
    round $
      (fromIntegral randomWord :: Double)
        / fromIntegral (maxBound :: RandomWord)
        * fromIntegral totalSize

splitWord64 :: Word64 -> (Word32, Word32)
splitWord64 w = (fromIntegral (w `shiftR` 32), fromIntegral w)
