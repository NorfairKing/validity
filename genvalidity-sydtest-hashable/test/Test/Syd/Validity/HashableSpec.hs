{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Test.Syd.Validity.HashableSpec where

import Data.GenValidity
import Data.Hashable
import GHC.Generics
import Test.Syd
import Test.Syd.Validity.Hashable

spec :: Spec
spec = do
  hashableSpecOnValid @Rational
#if MIN_VERSION_hashable(1,3,0)
  hashableSpecOnValid @Double
#endif
  hashableSpec @Int
  hashableSpecOnArbitrary @Int
  hashableSpec @HashableValid

newtype HashableValid
  = HashableValid Int
  deriving (Show, Generic)

hT :: Int -- Number used in the definition of HashableValid
hT = 7

instance Eq HashableValid where
  (==) (HashableValid x) (HashableValid y) = (x `mod` hT) == (y `mod` hT)

instance Hashable HashableValid where
  hashWithSalt n (HashableValid a) = (int ^ expo) `mod` hT
    where
      int = 1 + (a `mod` hT)
      expo = 1 + (n `mod` hT)

instance Validity HashableValid

instance GenValid HashableValid

instance GenUnchecked HashableValid

newtype HashableInvalid
  = HashableInvalid Int
  deriving (Show, Generic)

hF :: Int -- Numbers used in the definition of HashableInvalid
hF = 8

hM :: Int
hM = 3

instance Eq HashableInvalid where
  (==) (HashableInvalid x) (HashableInvalid y) = (x `mod` hF) == (y `mod` hF)

instance Hashable HashableInvalid where
  hashWithSalt n (HashableInvalid a) = (int ^ expo) `mod` hM
    where
      int = 1 + (a `mod` hM)
      expo = 1 + (n `mod` hM)

instance Validity HashableInvalid

instance GenValid HashableInvalid

instance GenUnchecked HashableInvalid
