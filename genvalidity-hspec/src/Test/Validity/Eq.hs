{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.Eq
    ( -- * Eq properties
      eqSpec
    , eqSpecOnArbitrary
    ) where

import           Data.Data
import           Data.Proxy

import           Data.GenValidity

import           Test.Hspec
import           Test.QuickCheck

import           Test.Validity.Functions
import           Test.Validity.Relations
import           Test.Validity.Utils

eqTypeStr :: Typeable a => Proxy a -> String
eqTypeStr = binRelStr "=="

neqTypeStr :: Typeable a => Proxy a -> String
neqTypeStr = binRelStr "/="

eqSpec
    :: (Show a, Eq a, Typeable a, GenValidity a)
    => Proxy a
    -> Spec
eqSpec proxy = eqSpecOnGen proxy genUnchecked

eqSpecOnArbitrary
    :: (Show a, Eq a, Typeable a, Arbitrary a)
    => Proxy a
    -> Spec
eqSpecOnArbitrary proxy = eqSpecOnGen proxy arbitrary

eqSpecOnGen
    :: (Show a, Eq a, Typeable a)
    => Proxy a
    -> Gen a
    -> Spec
eqSpecOnGen proxy gen = parallel $ do
    let name = nameOf proxy
        funeqstr = eqTypeStr proxy
        funneqstr = neqTypeStr proxy
        gen2 = (,) <$> gen <*> gen
        gen3 = (,,) <$> gen <*> gen <*> gen
    describe ("Eq " ++ name) $ do
        let eq a b = a `asProxyTypeOf` proxy == b
            neq a b = a `asProxyTypeOf` proxy /= b
        describe funeqstr $ do
            it "is reflexive" $
                reflexivityOnGen eq gen

            it "is symmetric" $
                symmetryOnGens eq gen2

            it "is transitive" $
                transitivityOnGens eq gen3

            it "is equivalent to (\\a b -> not $ a /= b)" $
                equivalentOnGens2 eq (\a b -> not $ a `neq` b) gen2

        describe funneqstr $ do
            it "is antireflexive" $
                antireflexivityOnGen neq gen

            it "is equivalent to (\\a b -> not $ a == b)" $
                equivalentOnGens2 neq (\a b -> not $ a `eq` b) gen2
