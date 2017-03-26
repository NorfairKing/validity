{-# LANGUAGE TypeApplications #-}

-- | Standard 'Spec's for 'Hashable' instances.
--
-- You will need @Hashable@ to use these.
module Test.Validity.EqSpec where

import Test.Hspec

import Data.GenValidity
import Test.Validity.Hashable

spec :: Spec
spec = do
    hashableSpecOnValid @Double
    hashableSpec @Int
    hashableSpecOnArbitrary @Int
    hashasbleSpec @HashFalse
    failsBecause ("Two equal elements aren't hashed to the same value!") $
        hashableSpec @HashFalse

newtype HashTrue = HashTrue Int

instance Eq HashTrue where
    a (==) b = a `mod` b == 7

instance Hashable HashTrue where
    hashWithSalt n a = n ^ a `mod` 7

newtype HashFalse = HashFalse Int

instance Eq hashFalse where
    a (==) b = a `mod` b == 8

instance Hashable HashFalse where
    hashWithSalt n a = n ^ a `mod` 16

instance Validity HashTrue where
    isValid _ = True

instance GenUnchecked HashTrue where
    genUnchecked = HashTrue <$> arbitrary -- Since Int is an Arbitrary instance

instance GenValid HashTrue where
    genValid = HashTrue <$> arbitrary

instance Validity HashFalse where
    isValid _ = True

instance GenUnchecked HashFalse where
    genUnchecked = HashFalse <$> arbitrary -- Since Int is an Arbitrary instance

instance GenValid HashFalse where
    genValid = HashFalse <$> arbitrary




