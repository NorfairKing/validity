{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
#if __GLASGOW_HASKELL__ >= 710
#define OVERLAPPING_ {-# OVERLAPPING #-}
#else
{-# LANGUAGE OverlappingInstances #-}
#define OVERLAPPING_
#endif
#if MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif
module Data.GenValidity.Utils
    ( -- ** Helper functions for implementing generators
      upTo
    , genSplit
    , genSplit3
    , genSplit4
    , genSplit5
    , genSplit6
    , genSplit7
    , genSplit8
    , arbPartition
    , shuffle
    , genListLength
    , genListOf
#if MIN_VERSION_base(4,9,0)
    , genNonEmptyOf
#endif
      -- ** Helper functions for implementing shrinking functions
    , shrinkTuple
    , shrinkT2
    , shrinkT3
    , shrinkT4
    , genIntX
    , genWordX
    ) where

import Test.QuickCheck hiding (Fixed)
import System.Random
#if !MIN_VERSION_QuickCheck(2,8,0)
import Data.List (sortBy)
import Data.Ord (comparing)
#endif
#if MIN_VERSION_base(4,9,0)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
#endif

#if MIN_VERSION_base(4,8,0)
import Control.Monad (forM, replicateM)
#else
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (forM, replicateM)
#endif
-- | 'upTo' generates an integer between 0 (inclusive) and 'n'.
upTo :: Int -> Gen Int
upTo n
    | n <= 0 = pure 0
    | otherwise = choose (0, n)

-- | 'genSplit a' generates a tuple '(b, c)' such that 'b + c' equals 'a'.
genSplit :: Int -> Gen (Int, Int)
genSplit n
    | n < 0 = pure (0, 0)
    | otherwise = do
        i <- choose (0, n)
        let j = n - i
        pure (i, j)

-- | 'genSplit3 a' generates a triple '(b, c, d)' such that 'b + c + d' equals 'a'.
genSplit3 :: Int -> Gen (Int, Int, Int)
genSplit3 n
    | n < 0 = pure (0, 0, 0)
    | otherwise = do
        (a, z) <- genSplit n
        (b, c) <- genSplit z
        return (a, b, c)

-- | 'genSplit4 a' generates a quadruple '(b, c, d, e)' such that 'b + c + d + e' equals 'a'.
genSplit4 :: Int -> Gen (Int, Int, Int, Int)
genSplit4 n
    | n < 0 = pure (0, 0, 0, 0)
    | otherwise = do
        (y, z) <- genSplit n
        (a, b) <- genSplit y
        (c, d) <- genSplit z
        return (a, b, c, d)

-- | 'genSplit5 a' generates a quintuple '(b, c, d, e, f)' such that 'b + c + d + e + f' equals 'a'.
genSplit5 :: Int -> Gen (Int, Int, Int, Int, Int)
genSplit5 n
    | n < 0 = pure (0, 0, 0, 0, 0)
    | otherwise = do
        (y, z) <- genSplit n
        (a, b, c) <- genSplit3 y
        (d, e) <- genSplit z
        return (a, b, c, d, e)

-- | 'genSplit6 a' generates a sextuple '(b, c, d, e, f, g)' such that 'b + c + d + e + f + g' equals 'a'.
genSplit6 :: Int -> Gen (Int, Int, Int, Int, Int, Int)
genSplit6 n
    | n < 0 = pure (0, 0, 0, 0, 0, 0)
    | otherwise = do
        (y, z) <- genSplit n
        (a, b, c) <- genSplit3 y
        (d, e, f) <- genSplit3 z
        return (a, b, c, d, e, f)

-- | 'genSplit7 a' generates a septtuple '(b, c, d, e, f, g)' such that 'b + c + d + e + f + g' equals 'a'.
genSplit7 :: Int -> Gen (Int, Int, Int, Int, Int, Int, Int)
genSplit7 n
    | n < 0 = pure (0, 0, 0, 0, 0, 0, 0)
    | otherwise = do
        (y, z) <- genSplit n
        (a, b, c) <- genSplit3 y
        (d, e, f, g) <- genSplit4 z
        return (a, b, c, d, e, f, g)

-- | 'genSplit8 a' generates a octtuple '(b, c, d, e, f, g, h)' such that 'b + c + d + e + f + g + h' equals 'a'.
genSplit8 :: Int -> Gen (Int, Int, Int, Int, Int, Int, Int, Int)
genSplit8 n
    | n < 0 = pure (0, 0, 0, 0, 0, 0, 0, 0)
    | otherwise = do
        (y, z) <- genSplit n
        (a, b, c, d) <- genSplit4 y
        (e, f, g, h) <- genSplit4 z
        return (a, b, c, d, e, f, g, h)


-- | 'arbPartition n' generates a list 'ls' such that 'sum ls' equals 'n', approximately.
arbPartition :: Int -> Gen [Int]
arbPartition 0 = pure []
arbPartition i = genListLengthWithSize i >>= go i
  where

    go :: Int -> Int -> Gen [Int]
    go size len = do
      us <- replicateM len $ choose (0, 1)
      let invs = map (invE 0.25) us
      -- Rescale the sizes to (approximately) sum to the given size.
      pure $ map (round . (* (fromIntegral size / sum invs))) invs

    -- Use an exponential distribution for generating the
    -- sizes in the partition.
    invE :: Double -> Double -> Double
    invE lambda u = - log (1 - u) / lambda

#if !MIN_VERSION_QuickCheck(2,8,0)
-- | Generates a random permutation of the given list.
shuffle :: [a] -> Gen [a]
shuffle xs = do
    ns <- vectorOf (length xs) (choose (minBound :: Int, maxBound))
    return (map snd (sortBy (comparing fst) (zip ns xs)))
#endif

#if MIN_VERSION_base(4,9,0)
genNonEmptyOf :: Gen a -> Gen (NonEmpty a)
genNonEmptyOf gen = do
  l <- genListOf gen
  case NE.nonEmpty l of
    Nothing -> scale (+1) $ genNonEmptyOf gen
    Just ne -> pure ne
#endif

-- Uses 'genListLengthWithSize' with the size parameter
genListLength :: Gen Int
genListLength = sized genListLengthWithSize

-- Generate a list length with the given size
genListLengthWithSize :: Int -> Gen Int
genListLengthWithSize maxLen = round . invT (fromIntegral maxLen) <$> choose (0, 1)
  where
    -- Use a triangle distribution for generating the
    -- length of the list
    -- with minimum length '0', mode length '2'
    -- and given max length.
    invT :: Double -> Double -> Double
    invT m u =
      let a = 0
          b = m
          c = 2
          fc = (c - a) / (b - a)
      in if u < fc
        then a + sqrt (u * (b - a) * (c - a) )
        else b - sqrt ((1 - u) * (b - a) * (b - c))


-- | A version of @listOf@ that takes size into account more accurately.
--
-- This generator distributes the size that is is given among the values
-- in the list that it generates.
genListOf :: Gen a -> Gen [a]
genListOf func =
    sized $ \n -> do
        pars <- arbPartition n
        forM pars $ \i -> resize i func

shrinkTuple :: (a -> [a]) -> (b -> [b]) -> (a, b) -> [(a, b)]
shrinkTuple sa sb (a, b) =
  ((,) <$> sa a <*> sb b)
  ++ [ (a', b) | a' <- sa a ]
  ++ [ (a, b') | b' <- sb b ]

-- | Turn a shrinking function into a function that shrinks tuples.
shrinkT2 :: (a -> [a]) -> (a, a) -> [(a, a)]
shrinkT2 s (a, b) = (,) <$> s a <*> s b

-- | Turn a shrinking function into a function that shrinks triples.
shrinkT3 :: (a -> [a]) -> (a, a, a) -> [(a, a, a)]
shrinkT3 s (a, b, c) = (,,) <$> s a <*> s b <*> s c

-- | Turn a shrinking function into a function that shrinks quadruples.
shrinkT4 :: (a -> [a]) -> (a, a, a, a) -> [(a, a, a, a)]
shrinkT4 s (a, b, c, d) = (,,,) <$> s a <*> s b <*> s c <*> s d

-- | Generate Int, Int8, Int16, Int32 and Int64 values smartly.
--
-- * Some at the border
-- * Some around zero
-- * Mostly uniformly
genIntX :: forall a. (Integral a, Bounded a, Random a) => Gen a
genIntX =
  frequency
    [ (1, extreme)
    , (1, small)
    , (8, uniform)
    ]
  where
    extreme :: Gen a
    extreme = sized $ \s -> oneof
      [ choose (maxBound - fromIntegral s, maxBound)
      , choose (minBound, minBound + fromIntegral s)
      ]
    small :: Gen a
    small = sized $ \s -> choose (- fromIntegral s, fromIntegral s)
    uniform :: Gen a
    uniform = choose (minBound, maxBound)

-- | Generate Word, Word8, Word16, Word32 and Word64 values smartly.
--
-- * Some at the border
-- * Some around zero
-- * Mostly uniformly
genWordX :: forall a. (Integral a, Bounded a, Random a) => Gen a
genWordX =
  frequency
    [ (1, extreme)
    , (1, small)
    , (8, uniform)
    ]
  where
    extreme :: Gen a
    extreme = sized $ \s ->
      choose (maxBound - fromIntegral s, maxBound)
    small :: Gen a
    small = sized $ \s -> choose (0, fromIntegral s)
    uniform :: Gen a
    uniform = choose (minBound, maxBound)
