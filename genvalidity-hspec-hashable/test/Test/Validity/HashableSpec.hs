{-# LANGUAGE TypeApplications #-}

-- | Standard 'Spec's for 'Hashable' instances.
--
-- You will need @Hashable@ to use these.
module Test.Validity.HashableSpec where

import Test.Hspec
import Data.Hashable
import Test.QuickCheck
import Test.Validity.Utils

import Data.GenValidity
import Test.Validity.Hashable

spec :: Spec
spec = do
    hashableSpecOnValid @Double
    hashableSpec @Int
    hashableSpecOnArbitrary @Int
    hashableSpec @HashTrue
    failsBecause ("Two equal elements aren't hashed to the same value!") $
        hashableSpec @HashFalse

newtype HashTrue = HashTrue Int

hT :: Int -- Number used in the definition of HashTrue
hT = 7

instance Eq HashTrue where
    (==) (HashTrue x) (HashTrue y) = (x `mod` hT) == (y `mod` hT)

instance Show HashTrue where
    showsPrec int (HashTrue a) = showsPrec int a

instance Hashable HashTrue where
    hashWithSalt n (HashTrue a) = (int ^ expo) `mod` hT
                               where int = 1 + (a `mod` hT)
                                     expo = 1 + (n `mod` hT)

instance Validity HashTrue where
    isValid _ = True

instance GenUnchecked HashTrue where
    genUnchecked = HashTrue <$> arbitrary -- Since Int is an Arbitrary instance

instance GenValid HashTrue where
    genValid = HashTrue <$> arbitrary

-- instance Typeable HashTrue where
--     typeRep# (HashTrue int) = typeRep# int

newtype HashFalse = HashFalse Int

hF :: Int -- Numbers used in the definition of HashFalse
hF = 8
hM :: Int
hM = 3

instance Show HashFalse where
    showsPrec int (HashFalse a) = showsPrec int a

instance Eq HashFalse where
    (==) (HashFalse x) (HashFalse y) = (x `mod` hF) == (y `mod` hF)

instance Hashable HashFalse where
    hashWithSalt n (HashFalse a) = (int ^ expo) `mod` hM
                                where int = 1 + (a `mod` hM)
                                      expo = 1 + (n `mod` hM)

instance GenValid HashFalse where
    genValid = HashFalse <$> arbitrary

instance Validity HashFalse where
    isValid _ = True

instance GenUnchecked HashFalse where
    genUnchecked = HashFalse <$> arbitrary -- Since Int is an Arbitrary instance

-- instance Typeable HashFalse where
--     typeRep# (HashFalse int) = typeRep# int



