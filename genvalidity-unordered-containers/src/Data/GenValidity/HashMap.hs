{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.HashMap where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure)
import Data.Functor ((<$>))
#endif
import Data.GenValidity
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.Validity.HashMap ()
import Test.QuickCheck

instance
  (Hashable k, Eq k, GenUnchecked k, GenUnchecked v) =>
  GenUnchecked (HashMap k v)
  where
  genUnchecked = HM.fromList <$> genUnchecked
  shrinkUnchecked = fmap HM.fromList . shrinkUnchecked . HM.toList

instance
  (Hashable k, Eq k, GenValid k, GenValid v) =>
  GenValid (HashMap k v)
  where
  genValid = HM.fromList <$> genValid
  shrinkValid = fmap HM.fromList . shrinkValid . HM.toList

instance
  (Hashable k, Eq k, GenUnchecked k, GenInvalid k, GenUnchecked v, GenInvalid v) =>
  GenInvalid (HashMap k v)
  where
  genInvalid =
    sized $ \n -> do
      (k, v, m) <- genSplit3 n
      let go g1 g2 = do
            key <- resize k g1
            val <- resize v g2
            rest <- resize m genUnchecked
            pure $ HM.insert key val rest
      oneof [go genInvalid genUnchecked, go genUnchecked genInvalid]

-- Note: HM.fromList <$> genInvalid does not work because of this line in the Data.HashMap documentation:
-- 'If the list contains duplicate mappings, the later mappings take precedence.'
