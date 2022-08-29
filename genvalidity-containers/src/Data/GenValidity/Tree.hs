{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Tree (genTreeOf, shrinkTreeOf) where

import Data.GenValidity
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Tree
import Data.Validity.Tree ()
import Test.QuickCheck

instance GenValid a => GenValid (Tree a) where
  genValid = genTreeOf genValid
  shrinkValid = shrinkTreeOf shrinkValid

shrinkTreeOf :: (a -> [a]) -> Tree a -> [Tree a]
shrinkTreeOf shrinker (Node v ts) =
  [Node v' ts' | (v', ts') <- shrinkTuple shrinker (shrinkList (shrinkTreeOf shrinker)) (v, ts)]

-- | Generate a tree of values that are generated as specified.
--
-- This takes the size parameter much better into account
genTreeOf :: Gen a -> Gen (Tree a)
genTreeOf func = do
  ne <- genNonEmptyOf func
  turnIntoTree ne
  where
    turnIntoTree :: NonEmpty a -> Gen (Tree a)
    turnIntoTree (e :| es) = do
      groups <- turnIntoGroups es
      subtrees <- mapM turnIntoTree groups
      pure (Node e subtrees)

    turnIntoGroups :: [a] -> Gen [NonEmpty a]
    turnIntoGroups = go []
      where
        go :: [a] -> [a] -> Gen [NonEmpty a]
        go acc [] =
          case NE.nonEmpty acc of
            Nothing -> pure []
            Just ne -> pure [ne]
        go acc (e : es) =
          frequency
            [ ( 1,
                do
                  rest <- go [] es
                  pure ((e :| acc) : rest)
              ),
              (4, go (e : acc) es)
            ]
