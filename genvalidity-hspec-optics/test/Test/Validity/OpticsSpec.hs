{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Validity.OpticsSpec where

import GHC.Generics (Generic)
import Lens.Micro
import Test.Hspec
import Test.Validity
import Test.Validity.Optics

spec :: Spec
spec = do
  describe "lensSpec" $ do
    lensSpec ((_2) :: Lens (Int, Int) (Int, Int) Int Int)
    lensSpec ((_2) :: Lens (Rational, Rational) (Rational, Rational) Rational Rational)
  describe "lensSpecOnArbitrary" $
    lensSpecOnArbitrary
      ((_2) :: Lens (Rational, Rational) (Rational, Rational) Rational Rational)
  describe "lensSpecOnGen" $
    lensSpecOnGen
      ((_2) :: Lens (Rational, Rational) (Rational, Rational) Rational Rational)
      (abs <$> genValid)
      "positive valid doubles"
      (filter (0.0 >=) . shrinkValid)
      ((,) <$> (negate . abs <$> genValid) <*> (negate . abs <$> genValid))
      "tuples of negative valid doubles"
      (const [])
  describe "lensGettingProducesValid" $ do
    it "holds for (_2) for ints" $
      lensGettingProducesValid ((_2) :: Lens (Int, Int) (Int, Int) Int Int)
    it "holds for (_2) for doubles" $
      lensGettingProducesValid
        ((_2) :: Lens (Rational, Rational) (Rational, Rational) Rational Rational)
  describe "lensGettingProducesValidOnArbitrary" $
    it "holds for (_2) for doubles" $
      lensGettingProducesValidOnArbitrary
        ((_2) :: Lens (Rational, Rational) (Rational, Rational) Rational Rational)
  describe "lensGettingProducesValidOnGen" $
    it "holds for (_2) for special generators" $
      lensGettingProducesValidOnGen
        ((_2) :: Lens (Rational, Rational) (Rational, Rational) Rational Rational)
        ((,) <$> (negate . abs <$> genValid) <*> (negate . abs <$> genValid))
        (const [])
  describe "lensSettingProducesValid" $ do
    it "holds for (_2) for ints" $
      lensSettingProducesValid ((_2) :: Lens (Int, Int) (Int, Int) Int Int)
    it "holds for (_2) for doubles" $
      lensSettingProducesValid ((_2) :: Lens (Rational, Rational) (Rational, Rational) Rational Rational)
  describe "lensSettingProducesValidOnArbitrary" $
    it "holds for (_2) for doubles" $
      lensSettingProducesValidOnArbitrary
        ((_2) :: Lens (Rational, Rational) (Rational, Rational) Rational Rational)
  describe "lensSettingProducesValidOnGen" $
    it "holds for (_2) for special generators" $
      lensSettingProducesValidOnGen
        ((_2) :: Lens (Rational, Rational) (Rational, Rational) Rational Rational)
        (abs <$> genValid)
        (filter (0.0 >=) . shrinkValid)
        ((,) <$> (negate . abs <$> genValid) <*> (negate . abs <$> genValid))
        (const [])
  describe "myBoolLens" $
    lensSpec myBoolLens
  describe "myRationalLens" $
    lensSpec myRationalLens

data MyRecord = MyRecord
  { myBool :: Bool,
    myRational :: Rational
  }
  deriving (Show, Eq, Generic)

instance Validity MyRecord

instance GenValid MyRecord

myBoolLens :: Lens' MyRecord Bool
myBoolLens = lens myBool $ \mr b -> mr {myBool = b}

myRationalLens :: Lens' MyRecord Rational
myRationalLens = lens myRational $ \mr r -> mr {myRational = r}
