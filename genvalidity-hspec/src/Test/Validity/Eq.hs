{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.Eq
    ( -- * Eq properties
      eqSpec
    , arbitraryEqSpec
    ) where

import           Data.Data
import           Data.Proxy

import           Data.GenValidity

import           Test.Hspec
import           Test.QuickCheck

import           Test.Validity.Functions
import           Test.Validity.Relations
import           Test.Validity.Utils

eqSpec
    :: (Show a, Eq a, Typeable a, GenValidity a)
    => Proxy a
    -> Spec
eqSpec proxy = do
    let name = nameOf proxy
    describe ("Eq " ++ name) $ do
        let funeqstr = unwords
              [ "(==) ::"
              , name
              , "->"
              , name
              , "-> Bool"
              ]
            eq a b = a `asProxyTypeOf` proxy == b
        describe funeqstr $ do
            it "is reflexive" $
                reflexivity eq

            it "is symmetric" $
                symmetry eq

            it "is transitive" $
                transitivity eq

        let funneqstr = unwords
              [ "(/=) ::"
              , name
              , "->"
              , name
              , "-> Bool"
              ]
            neq a b = a `asProxyTypeOf` proxy /= b
        describe funneqstr $
            it "is equivalent to (\\a b -> not $ a == b)" $
                equivalent2 neq (\a b -> not $ a `eq` b)

arbitraryEqSpec
    :: (Show a, Eq a, Typeable a, Arbitrary a)
    => Proxy a
    -> Spec
arbitraryEqSpec proxy = do
    let name = nameOf proxy
    describe ("Eq " ++ name) $ do
        let funeqstr = unwords
              [ "(==) ::"
              , name
              , "->"
              , name
              , "-> Bool"
              ]
            eq a b = a `asProxyTypeOf` proxy == b
        describe funeqstr $ do
            it "is reflexive" $
                reflexivityOnArbitrary eq

            it "is symmetric" $
                symmetryOnArbitrary eq

            it "is transitive" $
                transitivityOnArbitrary eq

        let funneqstr = unwords
              [ "(/=) ::"
              , name
              , "->"
              , name
              , "-> Bool"
              ]
            neq a b = a `asProxyTypeOf` proxy /= b
        describe funneqstr $
            it "is equivalent to (\\a b -> not $ a == b)" $
                equivalentOnArbitrary2 neq (\a b -> not $ a `eq` b)
