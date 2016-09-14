{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.Operations.Identity
    ( -- ** Identity element
      leftIdentityOnGen
    , leftIdentityOnValid
    , leftIdentity
    , rightIdentityOnGen
    , rightIdentityOnValid
    , rightIdentity
    , identityOnGen
    , identityOnValid
    , identity
    ) where

import           Data.GenValidity

import           Test.Hspec
import           Test.QuickCheck

leftIdentityOnGen
    :: (Show a, Eq a)
    => (b -> a -> a)
    -> b
    -> Gen a
    -> Property
leftIdentityOnGen op b gen =
    forAll gen $ \a ->
        b `op` a `shouldBe` a

leftIdentityOnValid
    :: (Show a, Eq a, GenValidity a)
    => (b -> a -> a)
    -> b
    -> Property
leftIdentityOnValid op b
    = leftIdentityOnGen op b genValid

leftIdentity
    :: (Show a, Eq a, GenValidity a)
    => (b -> a -> a)
    -> b
    -> Property
leftIdentity op b
    = leftIdentityOnGen op b genUnchecked

rightIdentityOnGen
    :: (Show a, Eq a)
    => (a -> b -> a)
    -> b
    -> Gen a
    -> Property
rightIdentityOnGen op b gen =
    forAll gen $ \a ->
        a `op` b `shouldBe` a

rightIdentityOnValid
    :: (Show a, Eq a, GenValidity a)
    => (a -> b -> a)
    -> b
    -> Property
rightIdentityOnValid op b
    = rightIdentityOnGen op b genValid

rightIdentity
    :: (Show a, Eq a, GenValidity a)
    => (a -> b -> a)
    -> b
    -> Property
rightIdentity op b
    = rightIdentityOnGen op b genUnchecked

identityOnGen
    :: (Show a, Eq a)
    => (a -> a -> a)
    -> a
    -> Gen a
    -> Property
identityOnGen op e gen =
    leftIdentityOnGen op e gen .&&. rightIdentityOnGen op e gen

identityOnValid
    :: (Show a, Eq a, GenValidity a)
    => (a -> a -> a)
    -> a
    -> Property
identityOnValid op a
    = identityOnGen op a genValid

identity
    :: (Show a, Eq a, GenValidity a)
    => (a -> a -> a)
    -> a
    -> Property
identity op e
    = identityOnGen op e genUnchecked
