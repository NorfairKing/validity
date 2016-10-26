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

{-# ANN module "HLint: ignore Use <=" #-}
{-# ANN module "HLint: ignore Use >=" #-}

leTypeStr :: Typeable a => Proxy a -> String
leTypeStr proxy = unwords
    [ "(<=) ::"
    , name
    , "->"
    , name
    , "-> Bool"
    ]
  where name = nameOf proxy

geTypeStr :: Typeable a => Proxy a -> String
geTypeStr proxy = unwords
    [ "(>=) ::"
    , name
    , "->"
    , name
    , "-> Bool"
    ]
  where name = nameOf proxy

ordSpec
    :: (Show a, Eq a, Ord a, Typeable a, GenValidity a)
    => Proxy a
    -> Spec
ordSpec proxy = do
    let name = nameOf proxy
        funlestr = leTypeStr proxy
        fungestr = geTypeStr proxy
        cmple a b = a `asProxyTypeOf` proxy <= b
        cmpge a b = a `asProxyTypeOf` proxy >= b

    describe ("Ord " ++ name) $ do
        describe funlestr $ do
            it "is reflexive" $
                reflexivity cmple

            it "is antisymmetric" $
                antisymmetry cmple

            it "is transitive" $
                transitivity cmple

            it "is equivalent to (\\a b -> compare a b /= GT)" $
                equivalent2 cmple (\a b -> compare a b /= GT)

        describe fungestr $ do
            it "is reflexive" $
                reflexivity cmpge

            it "is antisymmetric" $
                antisymmetry cmpge

            it "is transitive" $
                transitivity cmpge

            it "is equivalent to (\\a b -> compare a b /= LT)" $
                equivalent2 cmpge (\a b -> compare a b /= LT)

arbitraryOrdSpec
    :: (Show a, Eq a, Ord a, Typeable a, Arbitrary a)
    => Proxy a
    -> Spec
arbitraryOrdSpec proxy = do
    let name = nameOf proxy
        funlestr = leTypeStr proxy
        fungestr = geTypeStr proxy
        cmple a b = a `asProxyTypeOf` proxy <= b
        cmpge a b = a `asProxyTypeOf` proxy >= b

    describe ("Ord " ++ name) $ do
        describe funlestr $ do
            it "is reflexive" $
                reflexivityOnArbitrary cmple

            it "is antisymmetric" $
                antisymmetryOnArbitrary cmple

            it "is transitive" $
                transitivityOnArbitrary cmple

            it "is equivalent to (\\a b -> compare a b /= GT)" $
                equivalentOnArbitrary2 cmple (\a b -> compare a b /= GT)

        describe fungestr $ do
            it "is reflexive" $
                reflexivityOnArbitrary cmpge

            it "is antisymmetric" $
                antisymmetryOnArbitrary cmpge

            it "is transitive" $
                transitivityOnArbitrary cmpge

            it "is equivalent to (\\a b -> compare a b /= LT)" $
                equivalentOnArbitrary2 cmpge (\a b -> compare a b /= LT)
