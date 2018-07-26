{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Validity.OpticsSpec where

import Test.Hspec

import Test.Validity
import Test.Validity.Optics

import Lens.Micro

spec :: Spec
spec = do
    lensSpecOnValid
        ((_2) :: Lens (Double, Double) (Double, Double) Double Double)
    lensSpec ((_2) :: Lens (Int, Int) (Int, Int) Int Int)
    lensSpecOnArbitrary
        ((_2) :: Lens (Double, Double) (Double, Double) Double Double)
    lensSpecOnGen
        ((_2) :: Lens (Double, Double) (Double, Double) Double Double)
        (abs <$> genValid)
        "positive valid doubles"
        (filter (0.0 >=) . shrinkValid)
        ((,) <$> (negate . abs <$> genValid) <*> (negate . abs <$> genValid))
        "tuples of negative valid doubles"
        (const [])
