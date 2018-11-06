{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Validity.Operations.Identity
    ( leftIdentityOnElemWithEquality
    , leftIdentityOnGenWithEquality
    , leftIdentityOnGen
    , leftIdentityOnValid
    , leftIdentity
    , leftIdentityOnArbitrary
    , rightIdentityOnElemWithEquality
    , rightIdentityOnGenWithEquality
    , rightIdentityOnGen
    , rightIdentityOnValid
    , rightIdentity
    , rightIdentityOnArbitrary
    , identityOnGen
    , identityOnValid
    , identity
    , identityOnArbitrary
    ) where

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
       (b -> a -> a) -- ^ A binary operation
    -> (a -> a -> Bool) -- ^ An equality
    -> b -- ^ A candidate left-identity
    -> a -- ^ An element
    -> Bool
leftIdentityOnElemWithEquality op eq b a = (b `op` a) `eq` a

leftIdentityOnGenWithEquality ::
       Show a
    => (b -> a -> a) -- ^ A binary operation
    -> (a -> a -> Bool) -- ^ An equality
    -> b -- ^ A candidate left-identity
    -> Gen a
    -> (a -> [a])
    -> Property
leftIdentityOnGenWithEquality op eq b gen s =
    forAllShrink gen s $ leftIdentityOnElemWithEquality op eq b

leftIdentityOnGen ::
       (Show a, Eq a)
    => (b -> a -> a) -- ^ A binary operation
    -> b -- ^ A candidate left-identity
    -> Gen a
    -> (a -> [a])
    -> Property
leftIdentityOnGen op = leftIdentityOnGenWithEquality op (==)

-- |
--
-- prop> leftIdentityOnValid (flip ((^) :: Rational -> Int -> Rational)) 1
leftIdentityOnValid ::
       (Show a, Eq a, GenValid a) => (b -> a -> a) -> b -> Property
leftIdentityOnValid op b = leftIdentityOnGen op b genValid shrinkValid

-- |
--
-- prop> leftIdentity (flip ((^) :: Int -> Int -> Int)) 1
leftIdentity :: (Show a, Eq a, GenUnchecked a) => (b -> a -> a) -> b -> Property
leftIdentity op b = leftIdentityOnGen op b genUnchecked shrinkUnchecked

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
       (a -> b -> a) -- ^ A binary operation
    -> (a -> a -> Bool) -- ^ An equality
    -> b -- ^ A candidate right-identity
    -> a -- ^ An element
    -> Bool
rightIdentityOnElemWithEquality op eq b a = (a `op` b) `eq` a

rightIdentityOnGenWithEquality ::
       Show a
    => (a -> b -> a) -- ^ A binary operation
    -> (a -> a -> Bool) -- ^ An equality
    -> b -- ^ A candidate right-identity
    -> Gen a
    -> (a -> [a])
    -> Property
rightIdentityOnGenWithEquality op eq b gen s =
    forAllShrink gen s $ rightIdentityOnElemWithEquality op eq b

rightIdentityOnGen ::
       (Show a, Eq a)
    => (a -> b -> a) -- ^ A binary operation
    -> b -- ^ A candidate right-identity
    -> Gen a
    -> (a -> [a])
    -> Property
rightIdentityOnGen op = rightIdentityOnGenWithEquality op (==)

-- |
--
-- prop> rightIdentityOnValid ((^) :: Rational -> Int -> Rational) 1
rightIdentityOnValid ::
       (Show a, Eq a, GenValid a) => (a -> b -> a) -> b -> Property
rightIdentityOnValid op b = rightIdentityOnGen op b genValid shrinkValid

-- |
--
-- prop> rightIdentity ((^) :: Int -> Int -> Int) 1
rightIdentity ::
       (Show a, Eq a, GenUnchecked a) => (a -> b -> a) -> b -> Property
rightIdentity op b = rightIdentityOnGen op b genUnchecked shrinkUnchecked

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
-- prop> identityOnValid ((*) :: Rational -> Rational -> Rational) 1
-- prop> identityOnValid ((+) :: Rational -> Rational -> Rational) 0
identityOnValid :: (Show a, Eq a, GenValid a) => (a -> a -> a) -> a -> Property
identityOnValid op a = identityOnGen op a genValid shrinkValid

-- |
--
-- prop> identity ((*) :: Int -> Int -> Int) 1
-- prop> identity ((+) :: Int -> Int -> Int) 0
identity :: (Show a, Eq a, GenUnchecked a) => (a -> a -> a) -> a -> Property
identity op e = identityOnGen op e genUnchecked shrinkUnchecked

-- |
--
-- prop> identityOnArbitrary ((*) :: Int -> Int -> Int) 1
-- prop> identityOnArbitrary ((+) :: Int -> Int -> Int) 0
identityOnArbitrary ::
       (Show a, Eq a, Arbitrary a) => (a -> a -> a) -> a -> Property
identityOnArbitrary op a = identityOnGen op a arbitrary shrink
