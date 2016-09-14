{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.Operations.Identity
    ( -- *** Left identity
      leftIdentityOnElemWithEquality
    , leftIdentityOnGenWithEquality
    , leftIdentityOnGen
    , leftIdentityOnValid
    , leftIdentity

      -- *** Right identity
    , rightIdentityOnElemWithEquality
    , rightIdentityOnGenWithEquality
    , rightIdentityOnGen
    , rightIdentityOnValid
    , rightIdentity

    , identityOnGen
    , identityOnValid
    , identity
    ) where

import           Data.GenValidity

import           Test.QuickCheck

-- |
--
-- \[
--   LeftIdentity(\star, \doteq, b)
--   \quad\equiv\quad
--   \forall a: (b \star a) \doteq a
-- \]
leftIdentityOnElemWithEquality
    :: (b -> a -> a)    -- ^ A binary operation
    -> (a -> a -> Bool) -- ^ An equality
    -> b                -- ^ A candidate left-identity
    -> a                -- ^ An element
    -> Bool
leftIdentityOnElemWithEquality op eq b a = (b `op` a) `eq` a

leftIdentityOnGenWithEquality
    :: Show a
    => (b -> a -> a)    -- ^ A binary operation
    -> (a -> a -> Bool) -- ^ An equality
    -> b                -- ^ A candidate left-identity
    -> Gen a
    -> Property
leftIdentityOnGenWithEquality op eq b gen =
    forAll gen $ leftIdentityOnElemWithEquality op eq b

leftIdentityOnGen
    :: (Show a, Eq a)
    => (b -> a -> a) -- ^ A binary operation
    -> b             -- ^ A candidate left-identity
    -> Gen a
    -> Property
leftIdentityOnGen op = leftIdentityOnGenWithEquality op (==)

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

-- |
--
-- \[
--   RightIdentity(\star, \doteq, b)
--   \quad\equiv\quad
--   \forall a: (a \star b) \doteq a
-- \]
rightIdentityOnElemWithEquality
    :: (a -> b -> a)    -- ^ A binary operation
    -> (a -> a -> Bool) -- ^ An equality
    -> b                -- ^ A candidate right-identity
    -> a                -- ^ An element
    -> Bool
rightIdentityOnElemWithEquality op eq b a = (a `op` b) `eq` a

rightIdentityOnGenWithEquality
    :: Show a
    => (a -> b -> a)    -- ^ A binary operation
    -> (a -> a -> Bool) -- ^ An equality
    -> b                -- ^ A candidate right-identity
    -> Gen a
    -> Property
rightIdentityOnGenWithEquality op eq b gen =
    forAll gen $ rightIdentityOnElemWithEquality op eq b

rightIdentityOnGen
    :: (Show a, Eq a)
    => (a -> b -> a)    -- ^ A binary operation
    -> b                -- ^ A candidate right-identity
    -> Gen a
    -> Property
rightIdentityOnGen op = rightIdentityOnGenWithEquality op (==)

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
