{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.Tree where
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>))
import Data.Functor ((<$>))
#endif
import Data.GenValidity
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Validity.Tree ()

import Test.QuickCheck

import Data.Tree

instance GenUnchecked a => GenUnchecked (Tree a) where
  genUnchecked = genTreeOf genUnchecked
  shrinkUnchecked (Node v ts) = [Node v' ts' | (v', ts') <- shrinkUnchecked (v, ts)]

instance GenValid a => GenValid (Tree a) where
  genValid = genTreeOf genValid
  shrinkValid (Node v ts) = [Node v' ts' | (v', ts') <- shrinkValid (v, ts)]

-- | There should be at least one invalid element, either it's here or it's
-- further down the tree.
instance (GenUnchecked a, GenInvalid a) => GenInvalid (Tree a) where
  genInvalid =
    sized $ \n -> do
      size <- upTo n
      (a, b) <- genSplit size
      oneof
        [ Node <$> resize a genInvalid <*> resize b genUnchecked
        , Node <$> resize a genUnchecked <*> resize b genInvalid
        ]
  shrinkInvalid (Node v ts) =
    if isInvalid v
      then Node <$> shrinkInvalid v <*> shrinkUnchecked ts
      else Node <$> shrinkUnchecked v <*> shrinkInvalid ts

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
            go acc (e:es) =
              frequency
                [ ( 1
                  , do rest <- go [] es
                       pure ((e :| acc) : rest))
                , (4, go (e : acc) es)
                ]
