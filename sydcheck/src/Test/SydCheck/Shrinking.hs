{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.SydCheck.Shrinking where

import Data.Maybe
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import Data.Word
import Test.SydCheck.Randomness

-- Pretend every shrunk version still fails.
computeAllShrinks :: Randomness -> [Randomness]
computeAllShrinks v =
  if nullRandomness v
    then []
    else
      let vs = shrinkRandomness v
       in case listToMaybe vs of
            Nothing -> []
            Just v' -> v' : computeAllShrinks v'

-- Compute the next shrinking steps.
--
-- Laws:
--
-- 1. Must never shrink to itself.
--
-- These needs to be in order of "must shrinking progress first"
shrinkRandomness :: Randomness -> [Randomness]
shrinkRandomness r@(Randomness ws) =
  concat
    [ every tryToLog,
      every tryToSqrt,
      every (tryToDivideBy 2),
      -- every tryToPred,
      shorteningsFromBack,
      shorteningsFromFront,
      each tryToLog,
      each tryToSqrt,
      each (tryToDivideBy 2),
      each tryToPred
    ]
  where
    -- TODO keep a cache of the randomnesses we've
    -- already tried
    every :: (Word64 -> Maybe Word64) -> [Randomness]
    every fun =
      let ws' = UV.map (\v -> fromMaybe v $ fun v) ws
       in [Randomness ws' | ws /= ws']
    shorteningsFromBack :: [Randomness]
    shorteningsFromBack = [takeRandomness l r | l <- [1 .. sizeRandomness r - 1]]
    shorteningsFromFront :: [Randomness]
    shorteningsFromFront = [dropRandomness l r | l <- [sizeRandomness r - 1, sizeRandomness r - 2 .. 1]]
    each :: (Word64 -> Maybe Word64) -> [Randomness]
    each fun =
      mapMaybe
        ( \ix -> do
            let v = UV.unsafeIndex ws ix
            v' <- fun v
            pure $
              Randomness $
                UV.modify
                  ( \mv ->
                      MUV.write mv ix v'
                  )
                  ws
        )
        [0 .. UV.length ws - 1]
    tryToLog = \case
      0 -> Nothing
      1 -> Nothing
      w -> Just $ floor (logBase 2 (fromIntegral w) :: Double)
    tryToSqrt = \case
      0 -> Nothing
      1 -> Nothing
      w -> Just $ floor (sqrt (fromIntegral w) :: Double)
    tryToDivideBy d = \case
      0 -> Nothing
      w -> Just $ w `div` d
    tryToPred = \case
      0 -> Nothing
      w -> Just $ pred w
