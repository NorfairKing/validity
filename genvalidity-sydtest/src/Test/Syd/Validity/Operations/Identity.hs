{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Validity.Operations.Identity
  ( leftIdentityOnElemWithEquality,
    leftIdentityOnGenWithEquality,
    leftIdentityOnGen,
    leftIdentity,
    leftIdentityOnArbitrary,
    rightIdentityOnElemWithEquality,
    rightIdentityOnGenWithEquality,
    rightIdentityOnGen,
    rightIdentity,
    rightIdentityOnArbitrary,
    identityOnGen,
    identity,
    identityOnArbitrary,
  )
where

import Data.GenValidity
import Test.QuickCheck

-- |
--
-- \[
--   LeftIdentity(\star, \doteq, b)
--   \quad\equiv\quad
--   \forall a: (b \star a) \doteq a
-- \]
leftIdentityOnElemWithEquality ::
  -- | A binary operation
  (b -> a -> a) ->
  -- | An equality
  (a -> a -> Bool) ->
  -- | A candidate left-identity
  b ->
  -- | An element
  a ->
  Bool
leftIdentityOnElemWithEquality op eq b a = (b `op` a) `eq` a

leftIdentityOnGenWithEquality ::
  (Show a) =>
  -- | A binary operation
  (b -> a -> a) ->
  -- | An equality
  (a -> a -> Bool) ->
  -- | A candidate left-identity
  b ->
  Gen a ->
  (a -> [a]) ->
  Property
leftIdentityOnGenWithEquality op eq b gen s =
  forAllShrink gen s $ leftIdentityOnElemWithEquality op eq b

leftIdentityOnGen ::
  (Show a, Eq a) =>
  -- | A binary operation
  (b -> a -> a) ->
  -- | A candidate left-identity
  b ->
  Gen a ->
  (a -> [a]) ->
  Property
leftIdentityOnGen op = leftIdentityOnGenWithEquality op (==)

-- |
--
-- prop> leftIdentity (flip ((^) :: Int -> Int -> Int)) 1
leftIdentity :: (Show a, Eq a, GenValid a) => (b -> a -> a) -> b -> Property
leftIdentity op b = leftIdentityOnGen op b genValid shrinkValid

-- |
--
-- prop> leftIdentityOnArbitrary (flip ((^) :: Int -> Int -> Int)) 1
leftIdentityOnArbitrary ::
  (Show a, Eq a, Arbitrary a) => (b -> a -> a) -> b -> Property
leftIdentityOnArbitrary op b = leftIdentityOnGen op b arbitrary shrink

-- |
--
-- \[
--   RightIdentity(\star, \doteq, b)
--   \quad\equiv\quad
--   \forall a: (a \star b) \doteq a
-- \]
rightIdentityOnElemWithEquality ::
  -- | A binary operation
  (a -> b -> a) ->
  -- | An equality
  (a -> a -> Bool) ->
  -- | A candidate right-identity
  b ->
  -- | An element
  a ->
  Bool
rightIdentityOnElemWithEquality op eq b a = (a `op` b) `eq` a

rightIdentityOnGenWithEquality ::
  (Show a) =>
  -- | A binary operation
  (a -> b -> a) ->
  -- | An equality
  (a -> a -> Bool) ->
  -- | A candidate right-identity
  b ->
  Gen a ->
  (a -> [a]) ->
  Property
rightIdentityOnGenWithEquality op eq b gen s =
  forAllShrink gen s $ rightIdentityOnElemWithEquality op eq b

rightIdentityOnGen ::
  (Show a, Eq a) =>
  -- | A binary operation
  (a -> b -> a) ->
  -- | A candidate right-identity
  b ->
  Gen a ->
  (a -> [a]) ->
  Property
rightIdentityOnGen op = rightIdentityOnGenWithEquality op (==)

-- |
--
-- prop> rightIdentity ((^) :: Int -> Int -> Int) 1
rightIdentity ::
  (Show a, Eq a, GenValid a) => (a -> b -> a) -> b -> Property
rightIdentity op b = rightIdentityOnGen op b genValid shrinkValid

-- |
--
-- prop> rightIdentityOnArbitrary ((^) :: Int -> Int -> Int) 1
rightIdentityOnArbitrary ::
  (Show a, Eq a, Arbitrary a) => (a -> b -> a) -> b -> Property
rightIdentityOnArbitrary op b = rightIdentityOnGen op b arbitrary shrink

-- |
--
-- \[
--   Identity(\star, \doteq, b)
--   \quad\equiv\quad
--   LeftIdentity(\star, \doteq, b) \wedge RightIdentity(\star, \doteq, b)
-- \]
identityOnGen ::
  (Show a, Eq a) => (a -> a -> a) -> a -> Gen a -> (a -> [a]) -> Property
identityOnGen op e gen s =
  leftIdentityOnGen op e gen s .&&. rightIdentityOnGen op e gen s

-- |
--
-- prop> identity ((*) :: Int -> Int -> Int) 1
-- prop> identity ((+) :: Int -> Int -> Int) 0
identity :: (Show a, Eq a, GenValid a) => (a -> a -> a) -> a -> Property
identity op e = identityOnGen op e genValid shrinkValid

-- |
--
-- prop> identityOnArbitrary ((*) :: Int -> Int -> Int) 1
-- prop> identityOnArbitrary ((+) :: Int -> Int -> Int) 0
identityOnArbitrary ::
  (Show a, Eq a, Arbitrary a) => (a -> a -> a) -> a -> Property
identityOnArbitrary op a = identityOnGen op a arbitrary shrink
