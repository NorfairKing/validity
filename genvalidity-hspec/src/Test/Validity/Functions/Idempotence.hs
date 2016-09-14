{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.Functions.Idempotence
    ( -- ** Standard tests involving validity
      idempotentOnGen
    , idempotentOnValid
    , idempotent
    , idempotentOnArbitrary
    ) where

import           Data.GenValidity

import           Test.Hspec
import           Test.QuickCheck

idempotentOnGen
    :: (Show a, Eq a)
    => (a -> a)
    -> Gen a
    -> Property
idempotentOnGen f gen
    = forAll gen $ \a ->
        f (f a) `shouldBe` f a

idempotentOnValid
    :: (Show a, Eq a, GenValidity a)
    => (a -> a)
    -> Property
idempotentOnValid func = idempotentOnGen func genValid

idempotent
    :: (Show a, Eq a, GenValidity a)
    => (a -> a)
    -> Property
idempotent func = idempotentOnGen func genUnchecked

idempotentOnArbitrary
    :: (Show a, Eq a, Arbitrary a)
    => (a -> a)
    -> Property
idempotentOnArbitrary func = idempotentOnGen func arbitrary
