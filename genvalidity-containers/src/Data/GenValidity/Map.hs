{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.Map
    ( genStructurallyValidMapOf
    , genStructurallyValidMapOfInvalidValues
    ) where
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure)
import Data.Functor ((<$>))
#endif
import Data.GenValidity
import Data.Validity.Map ()
import Test.QuickCheck

import Data.Map (Map)
import qualified Data.Map as M
#if !MIN_VERSION_base(4,8,0)
import qualified Data.Map.Internal as Internal
#endif

#if !MIN_VERSION_base(4,8,0)
instance (Ord k, GenUnchecked k, GenUnchecked v) => GenUnchecked (Map k v) where
    genUnchecked =
        sized $ \n ->
            case n of
                0 -> pure Internal.Tip
                _ -> do
                    (a, b, c, d, e) <- genSplit5 n
                    Internal.Bin <$> resize a genUnchecked <*>
                        resize b genUnchecked <*>
                        resize c genUnchecked <*>
                        resize d genUnchecked <*>
                        resize e genUnchecked
    shrinkUnchecked Internal.Tip = []
    shrinkUnchecked (Internal.Bin s k a m1 m2) =
        Internal.Tip :
        [m1, m2] ++
        [ Internal.Bin s' k' a' m1' m2'
        | (s', k', a', m1', m2') <- shrinkUnchecked (s, k, a, m1, m2)
        ]
#else
instance (Ord k, GenUnchecked k, GenUnchecked v) => GenUnchecked (Map k v) where
    genUnchecked = M.fromList <$> genUnchecked
    shrinkUnchecked = fmap M.fromList . shrinkUnchecked . M.toList
#endif

instance (Ord k, GenValid k, GenValid v) => GenValid (Map k v) where
    genValid = M.fromList <$> genValid

instance (Ord k, GenInvalid k, GenInvalid v) => GenInvalid (Map k v) where
    genInvalid =
        oneof
            [genStructurallyValidMapOfInvalidValues, genStructurallyInvalidMap]

genStructurallyValidMapOf :: Ord k => Gen (k, v) -> Gen (Map k v)
genStructurallyValidMapOf g =
    sized $ \n ->
        case n of
            0 -> pure M.empty
            _ -> do
                (kv, m) <- genSplit n
                (key, val) <- resize kv g
                rest <- resize m $ genStructurallyValidMapOf g
                pure $ M.insert key val rest

-- Note: M.fromList <$> genInvalid does not work because of this line in the Data.Map documentation:
-- ' If the list contains more than one value for the same key, the last value for the key is retained.'
genStructurallyValidMapOfInvalidValues ::
       (Ord k, GenInvalid k, GenInvalid v) => Gen (Map k v)
genStructurallyValidMapOfInvalidValues =
    sized $ \n -> do
        (k, v, m) <- genSplit3 n
        let go g1 g2 = do
                key <- resize k g1
                val <- resize v g2
                rest <-
                    resize m $
                    genStructurallyValidMapOf $
                    (,) <$> genUnchecked <*> genUnchecked
                pure $ M.insert key val rest
        oneof [go genInvalid genUnchecked, go genUnchecked genInvalid]

genStructurallyInvalidMap ::
       (Ord k, GenUnchecked k, GenUnchecked v) => Gen (Map k v)
genStructurallyInvalidMap = do
    v <- genUnchecked
    if M.valid v
        then scale (+ 1) genUnchecked
        else pure v
