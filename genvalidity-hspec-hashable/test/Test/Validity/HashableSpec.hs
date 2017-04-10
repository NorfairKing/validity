{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Standard 'Spec's for 'Hashable' instances.
--
-- You will need @Hashable@ to use these.
module Test.Validity.HashableSpec where

import Test.Hspec
import Data.Hashable
import Test.Validity.Utils
import GHC.Generics

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
    deriving (Show, Generic)

hT :: Int -- Number used in the definition of HashTrue
hT = 7

instance Eq HashTrue where
    (==) (HashTrue x) (HashTrue y) = (x `mod` hT) == (y `mod` hT)

instance Hashable HashTrue where
    hashWithSalt n (HashTrue a) = (int ^ expo) `mod` hT
                               where int = 1 + (a `mod` hT)
                                     expo = 1 + (n `mod` hT)

instance Validity HashTrue

instance GenValid HashTrue

instance GenUnchecked HashTrue

newtype HashFalse = HashFalse Int
    deriving (Show, Generic)

hF :: Int -- Numbers used in the definition of HashFalse
hF = 8
hM :: Int
hM = 3

instance Eq HashFalse where
    (==) (HashFalse x) (HashFalse y) = (x `mod` hF) == (y `mod` hF)

instance Hashable HashFalse where
    hashWithSalt n (HashFalse a) = (int ^ expo) `mod` hM
                                where int = 1 + (a `mod` hM)
                                      expo = 1 + (n `mod` hM)

instance Validity HashFalse

instance GenValid HashFalse

instance GenUnchecked HashFalse


