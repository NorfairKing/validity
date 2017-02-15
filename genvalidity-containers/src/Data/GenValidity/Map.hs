{-# OPTIONS_GHC -Wno-orphans #-}

module Data.GenValidity.Map where

import Data.GenValidity
import Data.Validity.Map ()
import Test.QuickCheck

import Data.Map (Map)
import qualified Data.Map as M

instance (Ord k, GenUnchecked k, GenUnchecked v) =>
         GenUnchecked (Map k v) where
    genUnchecked = M.fromList <$> genUnchecked

instance (Ord k, GenValid k, GenValid v) =>
         GenValid (Map k v) where
    genValid = M.fromList <$> genValid

instance (Ord k, GenInvalid k, GenInvalid v) =>
         GenInvalid (Map k v) where
    genInvalid =
        sized $ \n -> do
            (k, v, m) <- genSplit3 n
            let go g1 g2 = do
                    key <- resize k g1
                    val <- resize v g2
                    rest <- resize m genUnchecked
                    pure $ M.insert key val rest
            oneof $ [go genInvalid genUnchecked, go genUnchecked genInvalid]
    -- Note: M.fromList <$> genInvalid does not work because of this line in the Data.Map documentation:
    -- ' If the list contains more than one value for the same key, the last value for the key is retained.'
