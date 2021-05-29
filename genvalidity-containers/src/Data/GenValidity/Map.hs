{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Map
  ( genMapOf,
    genStructurallyValidMapOf,
    genStructurallyValidMapOfInvalidValues,
#if MIN_VERSION_containers(0,5,9)
    genStructurallyInvalidMap,
#endif
  )
where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure, (<$>), (<*>))
#endif
import Data.GenValidity
import Data.Map (Map)
import qualified Data.Map as M
import Data.Validity.Map ()
import Test.QuickCheck

#if MIN_VERSION_containers(0,5,9)
import qualified Data.Map.Internal as Internal
#endif

#if MIN_VERSION_containers(0,5,9)
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
instance (Show k, Ord k, GenValid k, GenValid v) => GenValid (Map k v) where
  genValid = genMapOf genValid
  shrinkValid = fmap M.fromList . shrinkValid . M.toList

#if MIN_VERSION_containers(0,5,9)
instance (Show k, Ord k, GenUnchecked k, GenInvalid k, GenUnchecked v, GenInvalid v) => GenInvalid (Map k v) where
    genInvalid =
        oneof
            [genStructurallyValidMapOfInvalidValues, genStructurallyInvalidMap]
#else
instance (Show k, Ord k, GenUnchecked k, GenInvalid k, GenUnchecked v, GenInvalid v) => GenInvalid (Map k v) where
    genInvalid = genStructurallyValidMapOfInvalidValues
#endif

genMapOf :: Ord k => Gen (k, v) -> Gen (Map k v)
genMapOf = genStructurallyValidMapOf

genStructurallyValidMapOf :: Ord k => Gen (k, v) -> Gen (Map k v)
genStructurallyValidMapOf g = M.fromList <$> genListOf g

-- Note: M.fromList <$> genInvalid does not work because of this line in the Data.Map documentation:
-- ' If the list contains more than one value for the same key, the last value for the key is retained.'
genStructurallyValidMapOfInvalidValues ::
  (Ord k, GenUnchecked k, GenInvalid k, GenUnchecked v, GenInvalid v) => Gen (Map k v)
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

#if MIN_VERSION_containers(0,5,9)
genStructurallyInvalidMap ::
       (Show k, Ord k, GenUnchecked k, GenUnchecked v) => Gen (Map k v)
genStructurallyInvalidMap = do
    v <- genUnchecked
    if M.valid v
        then scale (+ 1) genStructurallyInvalidMap
        else pure v
#endif
