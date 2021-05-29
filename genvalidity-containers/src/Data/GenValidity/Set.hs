{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Set
  ( genSetOf,
    genStructurallyValidSetOf,
    genStructurallyValidSetOfInvalidValues,
#if MIN_VERSION_containers(0,5,9)
    , genStructurallyInvalidSet
#endif
    genSeperate,
    genSeperateFor,
    genSeperateForNE,
    genValidSeperateFor,
    genValidSeperateForNE,
  )
where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), pure)
#endif

import Data.Containers.ListUtils
import Data.GenValidity
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as S
import Data.Validity.Set ()
import Test.QuickCheck

#if MIN_VERSION_containers(0,5,9)
import qualified Data.Set.Internal as Internal
#endif
#if MIN_VERSION_containers(0,5,9)
instance (Ord v, GenUnchecked v) => GenUnchecked (Set v) where
    genUnchecked =
        sized $ \n ->
            case n of
                0 -> pure Internal.Tip
                _ -> do
                    (a, b, c, d) <- genSplit4 n
                    Internal.Bin <$> resize a genUnchecked <*>
                        resize b genUnchecked <*>
                        resize c genUnchecked <*>
                        resize d genUnchecked
    shrinkUnchecked Internal.Tip = []
    shrinkUnchecked (Internal.Bin s a s1 s2) =
        Internal.Tip :
        [s1, s2] ++
        [ Internal.Bin s' a' s1' s2'
        | (s', a', s1', s2') <- shrinkUnchecked (s, a, s1, s2)
        ]
#else
instance (Ord v, GenUnchecked v) => GenUnchecked (Set v) where
    genUnchecked = S.fromList <$> genUnchecked
    shrinkUnchecked = fmap S.fromList . shrinkUnchecked . S.toList
#endif
instance (Ord v, GenValid v) => GenValid (Set v) where
  genValid = S.fromList <$> genValid
  shrinkValid = fmap S.fromList . shrinkValid . S.toList

#if MIN_VERSION_containers(0,5,9)
instance (Ord v, GenUnchecked v, GenInvalid v) => GenInvalid (Set v) where
    genInvalid =
        oneof
            [genStructurallyValidSetOfInvalidValues, genStructurallyInvalidSet]
#else
instance (Ord v, GenUnchecked v, GenInvalid v) => GenInvalid (Set v) where
    genInvalid = genStructurallyValidSetOfInvalidValues
#endif

genSetOf :: Ord v => Gen v -> Gen (Set v)
genSetOf = genStructurallyValidSetOf

genStructurallyValidSetOf :: Ord v => Gen v -> Gen (Set v)
genStructurallyValidSetOf g = S.fromList <$> genListOf g

-- Note: M.fromList <$> genInvalid does not work because of this line in the Data.Set documentation:
-- ' If the list contains more than one value for the same key, the last value for the key is retained.'
genStructurallyValidSetOfInvalidValues :: (Ord v, GenUnchecked v, GenInvalid v) => Gen (Set v)
genStructurallyValidSetOfInvalidValues =
  sized $ \n -> do
    (v, m) <- genSplit n
    val <- resize v genInvalid
    rest <- resize m $ genStructurallyValidSetOf genUnchecked
    pure $ S.insert val rest

#if MIN_VERSION_containers(0,5,9)
genStructurallyInvalidSet :: (Ord v, GenUnchecked v) => Gen (Set v)
genStructurallyInvalidSet = do
    v <- genUnchecked
    if S.valid v
        then scale (+ 1) genStructurallyInvalidSet
        else pure v
#endif

genValidSeperateFor :: (GenValid b, Eq b) => [a] -> Gen [(b, a)]
genValidSeperateFor = genSeperateFor genValid

genValidSeperateForNE :: (GenValid b, Eq b) => NonEmpty a -> Gen (NonEmpty (b, a))
genValidSeperateForNE = genSeperateForNE genValid

#if MIN_VERSION_containers(0,6,0)
genSeperate :: Ord a => Gen a -> Gen [a]
genSeperate g = nubOrd <$> genListOf g
#else
genSeperate :: Eq a => Gen a -> Gen [a]
genSeperate g = nub <$> genListOf g
#endif

-- TODO these two can likely be optimised
genSeperateFor :: Eq b => Gen b -> [a] -> Gen [(b, a)]
genSeperateFor _ [] = pure []
genSeperateFor g (a : as) = NE.toList <$> genSeperateForNE g (a :| as)

genSeperateForNE :: Eq b => Gen b -> NonEmpty a -> Gen (NonEmpty (b, a))
genSeperateForNE g (a :| as) = do
  restTups <- genSeperateFor g as
  b <- g `suchThat` (`notElem` map fst restTups)
  pure ((b, a) :| restTups)
