{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.Set
    ( genStructurallyValidSetOf
    , genStructurallyValidSetOfInvalidValues
#if !MIN_VERSION_containers(0,5,9)
    , genStructurallyInvalidSet
#endif
    ) where
#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
#endif
import Data.GenValidity
import Data.Validity.Set ()
import Test.QuickCheck

import Data.Set (Set)
import qualified Data.Set as S
#if !MIN_VERSION_containers(0,5,9)
import qualified Data.Set.Internal as Internal
#endif
#if !MIN_VERSION_containers(0,5,9)
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
#if !MIN_VERSION_containers(0,5,9)
instance (Ord v, GenInvalid v) => GenInvalid (Set v) where
    genInvalid =
        oneof
            [genStructurallyValidSetOfInvalidValues, genStructurallyInvalidSet]
#else
instance (Ord v, GenInvalid v) => GenInvalid (Set v) where
    genInvalid = genStructurallyValidSetOfInvalidValues
#endif
genStructurallyValidSetOf :: Ord v => Gen v -> Gen (Set v)
genStructurallyValidSetOf g =
    sized $ \n ->
        case n of
            0 -> pure S.empty
            _ -> do
                (v, m) <- genSplit n
                val <- resize v g
                rest <- resize m $ genStructurallyValidSetOf g
                pure $ S.insert val rest

-- Note: M.fromList <$> genInvalid does not work because of this line in the Data.Set documentation:
-- ' If the list contains more than one value for the same key, the last value for the key is retained.'
genStructurallyValidSetOfInvalidValues :: (Ord v, GenInvalid v) => Gen (Set v)
genStructurallyValidSetOfInvalidValues =
    sized $ \n -> do
        (v, m) <- genSplit n
        val <- resize v genInvalid
        rest <- resize m $ genStructurallyValidSetOf genUnchecked
        pure $ S.insert val rest
#if !MIN_VERSION_containers(0,5,9)
genStructurallyInvalidSet :: (Ord v, GenUnchecked v) => Gen (Set v)
genStructurallyInvalidSet = do
    v <- genUnchecked
    if S.valid v
        then scale (+ 1) genUnchecked
        else pure v
#endif
