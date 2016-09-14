{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.Eq
    ( -- * Eq properties
      eqSpec
    ) where

import           Data.Data
import           Data.Proxy

import           Data.GenValidity

import           Test.Hspec

import           Test.Validity.Relations
import           Test.Validity.Utils

eqSpec
    :: (Show a, Eq a, Typeable a, GenValidity a)
    => Proxy a
    -> Spec
eqSpec proxy = do
    let name = nameOf proxy
        funeqstr = unwords
          [ "(==) ::"
          , name
          , "->"
          , name
          , "-> Bool"
          ]
        eq a b = a `asProxyTypeOf` proxy == b
    describe ("Eq " ++ name) $ do
        it ("is instantated such that "
            ++ funeqstr
            ++ " is reflexive") $
            reflexivity eq

        it ("is instantated such that "
            ++ funeqstr
            ++ " is symmetric") $
            symmetry eq

        it ("is instantated such that "
            ++ funeqstr
            ++ " is transitive") $
            transitivity eq

