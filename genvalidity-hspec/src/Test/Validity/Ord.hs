{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.Ord
    ( -- * Ord properties
      ordSpec
    , ordSpecOnArbitrary
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
{-# ANN module "HLint: ignore Use <" #-}
{-# ANN module "HLint: ignore Use >" #-}

leTypeStr :: Typeable a => Proxy a -> String
leTypeStr = binRelStr "<="

geTypeStr :: Typeable a => Proxy a -> String
geTypeStr = binRelStr ">="

ltTypeStr :: Typeable a => Proxy a -> String
ltTypeStr = binRelStr "<"

gtTypeStr :: Typeable a => Proxy a -> String
gtTypeStr = binRelStr ">"

ordSpecOnArbitrary
    :: (Show a, Ord a, Typeable a, Arbitrary a)
    => Proxy a
    -> Spec
ordSpecOnArbitrary proxy = ordSpecOnGen proxy arbitrary

ordSpec
    :: (Show a, Ord a, Typeable a, GenValidity a)
    => Proxy a
    -> Spec
ordSpec proxy = ordSpecOnGen proxy genUnchecked

ordSpecOnGen
    :: (Show a, Eq a, Ord a, Typeable a)
    => Proxy a
    -> Gen a
    -> Spec
ordSpecOnGen proxy gen = parallel $ do
    let name = nameOf proxy
        funlestr = leTypeStr proxy
        fungestr = geTypeStr proxy
        funltstr = ltTypeStr proxy
        fungtstr = gtTypeStr proxy
        cmple a b = a `asProxyTypeOf` proxy <= b
        cmpge a b = a `asProxyTypeOf` proxy >= b
        cmplt a b = a `asProxyTypeOf` proxy < b
        cmpgt a b = a `asProxyTypeOf` proxy > b
        gen2 = (,) <$> gen <*> gen
        gen3 = (,,) <$> gen <*> gen <*> gen

    describe ("Ord " ++ name) $ do
        describe funlestr $ do
            it "is reflexive" $
                reflexivityOnGen cmple gen

            it "is antisymmetric" $
                antisymmetryOnGens cmple gen2

            it "is transitive" $
                transitivityOnGens cmple gen3

            it "is equivalent to (\\a b -> compare a b /= GT)" $
                equivalentOnGens2 cmple (\a b -> compare a b /= GT) gen2

        describe fungestr $ do
            it "is reflexive" $
                reflexivityOnGen cmpge gen

            it "is antisymmetric" $
                antisymmetryOnGens cmpge gen2

            it "is transitive" $
                transitivityOnGens cmpge gen3

            it "is equivalent to (\\a b -> compare a b /= LT)" $
                equivalentOnGens2 cmpge (\a b -> compare a b /= LT) gen2

        describe funltstr $ do
            it "is antireflexive" $
                antireflexivityOnGen cmplt gen

            it "is transitive" $
                transitivityOnGens cmplt gen3

            it "is equivalent to (\\a b -> compare a b == LT)" $
                equivalentOnGens2 cmplt (\a b -> compare a b == LT) gen2

        describe fungtstr $ do
            it "is antireflexive" $
                antireflexivityOnGen cmpgt gen

            it "is transitive" $
                transitivityOnGens cmpgt gen3

            it "is equivalent to (\\a b -> compare a b == GT)" $
                equivalentOnGens2 cmpgt (\a b -> compare a b == GT) gen2
