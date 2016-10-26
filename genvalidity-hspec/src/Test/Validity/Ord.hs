{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.Ord
    ( -- * Ord properties
      ordSpec
    , arbitraryOrdSpec
    ) where

import           Data.Data
import           Data.Proxy

import           Data.GenValidity

import           Test.Hspec
import           Test.QuickCheck

import           Test.Validity.Functions
import           Test.Validity.Relations
import           Test.Validity.Utils

ordSpec
    :: (Show a, Eq a, Ord a, Typeable a, GenValidity a)
    => Proxy a
    -> Spec
ordSpec proxy = do
    let name = nameOf proxy
        funlestr = unwords
          [ "(<=) ::"
          , name
          , "->"
          , name
          , "-> Ordering"
          ]
        cmp a b = a `asProxyTypeOf` proxy <= b
    describe ("Ord " ++ name) $ do
        describe funlestr $ do
            it "is reflexive" $
                reflexivity cmp

            it "is antisymmetric" $
                antisymmetry cmp

            it "is transitive" $
                transitivity cmp

            it "is equivalent to (\\a b -> compare a b /= GT)" $
                equivalent2 cmp (\a b -> compare a b /= GT)

arbitraryOrdSpec
    :: (Show a, Eq a, Ord a, Typeable a, Arbitrary a)
    => Proxy a
    -> Spec
arbitraryOrdSpec proxy = do
    let name = nameOf proxy
        funlestr = unwords
          [ "(<=) ::"
          , name
          , "->"
          , name
          , "-> Ordering"
          ]
        cmp a b = a `asProxyTypeOf` proxy <= b
    describe ("Ord " ++ name) $ do
        describe funlestr $ do
            it "is reflexive" $
                reflexivityOnArbitrary cmp

            it "is antisymmetric" $
                antisymmetryOnArbitrary cmp

            it "is transitive" $
                transitivityOnArbitrary cmp

            it "is equivalent to (\\a b -> compare a b /= GT)" $
                equivalentOnArbitrary2 cmp (\a b -> compare a b /= GT)
