{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.Ord
    ( -- * Ord properties
      ordSpec
    ) where

import           Data.Data
import           Data.Proxy

import           Data.GenValidity

import           Test.Hspec

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
        it ("is instantated such that "
            ++ funlestr
            ++ " is reflexive") $
            reflexivity cmp

        it ("is instantated such that "
            ++ funlestr
            ++ " is antisymmetric") $
            antisymmetry cmp

        it ("is instantated such that "
            ++ funlestr
            ++ " is transitive") $
            transitivity cmp
