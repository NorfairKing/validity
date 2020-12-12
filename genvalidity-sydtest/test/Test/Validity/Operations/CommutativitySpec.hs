{-# LANGUAGE TypeApplications #-}

module Test.Validity.Operations.CommutativitySpec
    ( spec
    ) where

import Test.Syd
import Test.QuickCheck
import Data.GenValidity (GenUnchecked)

import Test.Validity.Operations.Commutativity (commutative)

spec :: Spec
spec = do
  describe "commutative" $ do
    specify "+ is commutative" $ commutative @Int (+)
    specify "* is commutative" $ commutative @Int (*)
    specify "dot product is commutative" $ commutative dotProduct

    specify "- is not commutative" $ notCommmutative @Int (-)
    specify "cross product is not commutative" $ notCommmutative crossProduct

notCommmutative :: (Show a, Show b, Eq b, GenUnchecked a) => (a -> a -> b) -> Property
notCommmutative op = expectFailure (commutative op)

type Point = (Int, Int)

dotProduct :: Point -> Point -> Int
dotProduct (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

crossProduct :: Point -> Point -> Int
crossProduct (x1, y1) (x2, y2) = x1 * y2 - x2 * y1
