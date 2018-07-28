{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Validity.OpticsSpec where

import GHC.Generics (Generic)

import Test.Hspec

import Test.Validity
import Test.Validity.Optics

import Lens.Micro

spec :: Spec
spec = do
    describe "lensSpecOnValid" $
        lensSpecOnValid
            ((_2) :: Lens (Double, Double) (Double, Double) Double Double)
    describe "lensSpec" $ lensSpec ((_2) :: Lens (Int, Int) (Int, Int) Int Int)
    describe "lensSpecOnArbitrary" $
        lensSpecOnArbitrary
            ((_2) :: Lens (Double, Double) (Double, Double) Double Double)
    describe "lensSpecOnGen" $
        lensSpecOnGen
            ((_2) :: Lens (Double, Double) (Double, Double) Double Double)
            (abs <$> genValid)
            "positive valid doubles"
            (filter (0.0 >=) . shrinkValid)
            ((,) <$> (negate . abs <$> genValid) <*> (negate . abs <$> genValid))
            "tuples of negative valid doubles"
            (const [])
    describe "myBoolLens" $
        lensSpec myBoolLens -- For any unchecked value, prefer this version if you can.
    describe "myRationalLens" $
        lensSpecOnValid myRationalLens -- Only for valid values

data MyRecord = MyRecord
    { myBool :: Bool
    , myRational :: Rational
    } deriving (Show, Eq, Generic)

instance Validity MyRecord

instance GenUnchecked MyRecord

instance GenValid MyRecord

myBoolLens :: Lens' MyRecord Bool
myBoolLens = lens myBool $ \mr b -> mr {myBool = b}

myRationalLens :: Lens' MyRecord Rational
myRationalLens = lens myRational $ \mr r -> mr {myRational = r}
