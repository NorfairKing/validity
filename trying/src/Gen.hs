{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Gen where

import Control.Monad
import Data.Maybe
import Data.Validity
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
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
        let (leftWs, rightWs) = computeSplitRandomness ws
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

computeSplitRandomness :: Vector Word64 -> (Vector Word64, Vector Word64)
computeSplitRandomness ws =
  let len = UV.length ws
   in case len of
        0 -> (UV.empty, UV.empty)
        1 -> (UV.empty, UV.empty)
        n ->
          let leftSize = computeSplit (pred len) (UV.head ws)
              restRandomness = UV.tail ws
           in UV.splitAt leftSize restRandomness

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
shrinkRandomness :: Vector Word64 -> [Vector Word64]
shrinkRandomness ws =
  concat
    [ every tryToLog,
      every tryToSqrt,
      every tryToDivideByTwo,
      every tryToPred,
      [UV.tail ws | not (UV.null ws)],
      [UV.init ws | not (UV.null ws)],
      each tryToLog,
      each tryToSqrt,
      each tryToDivideByTwo,
      each tryToPred
    ]
  where
    -- TODO keep a cache of the randomnesses we've
    -- already tried
    every :: (Word64 -> Maybe Word64) -> [Vector Word64]
    every fun =
      let ws' = UV.map (\v -> fromMaybe v $ fun v) ws
       in [ws' | ws /= ws']
    each :: (Word64 -> Maybe Word64) -> [Vector Word64]
    each fun =
      mapMaybe
        ( \ix -> do
            let v = UV.unsafeIndex ws ix
            v' <- fun v
            pure $
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
      w -> Just $ floor $ logBase 2 $ fromIntegral w
    tryToSqrt = \case
      0 -> Nothing
      1 -> Nothing
      w -> Just $ floor $ sqrt $ fromIntegral w
    tryToDivideByTwo = \case
      0 -> Nothing
      w -> Just $ w `div` 2
    tryToPred = \case
      0 -> Nothing
      w -> Just $ pred w

data Property ls where
  PropBool :: Bool -> Property '[]
  PropGen :: Gen a -> (a -> Property ls) -> Property (a ': ls)

data family PList (l :: [*])

data instance PList '[] = PNil

data instance PList (x ': xs) = x `PCons` PList xs

deriving instance Show (PList '[])

deriving instance (Show x, Show (PList xs)) => Show (PList (x ': xs))

deriving instance Eq (PList '[])

deriving instance (Eq x, Eq (PList xs)) => Eq (PList (x ': xs))

deriving instance Ord (PList '[])

deriving instance (Ord x, Ord (PList xs)) => Ord (PList (x ': xs))

runProperty ::
  Vector Word64 ->
  Property ls ->
  (PList ls, Bool)
runProperty = go
  where
    go :: Vector Word64 -> Property ls -> (PList ls, Bool)
    go ws = \case
      PropBool b -> (PNil, b)
      PropGen gen func ->
        let (usedRandomness, restRandomness) = computeSplitRandomness ws
            value = runGen gen usedRandomness
            (generateds, result) = go restRandomness (func value)
         in (PCons value generateds, result)

shrinkProperty ::
  forall ls.
  Show (PList ls) =>
  Vector Word64 ->
  Property ls ->
  Maybe (PList ls)
shrinkProperty r prop =
  case shrinkPropertyAsMuchAsPossible r prop of
    [] -> Nothing
    ls -> Just $ last ls

shrinkPropertyAsMuchAsPossible ::
  forall ls.
  Show (PList ls) =>
  Vector Word64 ->
  Property ls ->
  [PList ls]
shrinkPropertyAsMuchAsPossible r prop = go r
  where
    run :: Vector Word64 -> (PList ls, Bool)
    run ws' = runProperty ws' prop
    go :: Vector Word64 -> [PList ls]
    go ws =
      traceShow ws $
        let shrinks = shrinkPropertyOneStep ws prop
         in case listToMaybe shrinks of
              Nothing -> []
              Just (ws', values) ->
                trace ("Successfully shrunk " <> show ws <> " to " <> show ws' <> " yielding " <> show values) $
                  traceShowId values : go ws'

shrinkPropertyOneStep ::
  Show (PList ls) =>
  Vector Word64 ->
  Property ls ->
  [(Vector Word64, PList ls)]
shrinkPropertyOneStep ws prop = do
  ws' <- shrinkRandomness ws
  let (vals, result) = runProperty ws' prop
  traceShowM (ws', vals, result)
  guard $ not result
  pure (ws', vals)

class IsProperty ls a where
  toProperty :: a -> Property ls

instance IsProperty '[] Bool where
  toProperty = PropBool

instance (GenValid a, IsProperty ls b) => IsProperty (a ': ls) (a -> b) where
  toProperty func = PropGen genValid $ \a -> toProperty (func a)
