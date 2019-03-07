{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
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
    , arbPartition
    , shuffle
    , genListOf
      -- ** Helper functions for implementing shrinking functions
    , shrinkTuple
    , shrinkT2
    , shrinkT3
    , shrinkT4
    ) where

import Test.QuickCheck hiding (Fixed)
#if !MIN_VERSION_QuickCheck(2,8,0)
import Data.List (sortBy)
import Data.Ord (comparing)
#endif

#if MIN_VERSION_base(4,8,0)
import Control.Monad (forM)
#else
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (forM)
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

-- | 'arbPartition n' generates a list 'ls' such that 'sum ls' equals 'n'.
arbPartition :: Int -> Gen [Int]
arbPartition i = go i >>= shuffle
  where
    go k
        | k <= 0 = pure []
        | otherwise = do
            first <- choose (1, k)
            rest <- arbPartition $ k - first
            return $ first : rest
#if !MIN_VERSION_QuickCheck(2,8,0)
-- | Generates a random permutation of the given list.
shuffle :: [a] -> Gen [a]
shuffle xs = do
    ns <- vectorOf (length xs) (choose (minBound :: Int, maxBound))
    return (map snd (sortBy (comparing fst) (zip ns xs)))
#endif
-- | A version of @listOf@ that takes size into account more accurately.
--
-- This generator distributes the size that is is given among the values
-- in the list that it generates.
genListOf :: Gen a -> Gen [a]
genListOf func =
    sized $ \n -> do
        size <- upTo n
        pars <- arbPartition size
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
