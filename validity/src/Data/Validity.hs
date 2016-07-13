{-# LANGUAGE FlexibleInstances #-}

{-|

 @Validity@ is used to specify additional invariants upon values that are not
 enforced by the type system.

 Let's take an example.
 Suppose we were to implement a type @Prime@ that represents prime integers.

 If you were to completely enforce the invariant that the represented number is
 a prime, then we could use @Numeric.Natural@ and only store the index of the
 given prime in the infinite sequence of prime numbers.
 This is very safe but also very expensive if we ever want to use the number,
 because we would have to calculcate all the prime numbers until that index.

 Instead we choose to implement @Prime@ by a @newtype Prime = Prime Int@.
 Now we have to maintain the invariant that the @Int@ that we use to represent
 the prime is in fact positive and a prime.

 The @Validity@ typeclass allows us to specify this invariant (and enables
 testing via the @genvalidity@ libraries:
 https://hackage.haskell.org/package/genvalidity):

 > instance Validity Prime where
 >     isValid (Prime n) = isPrime n
 -}

module Data.Validity
    ( Validity(..)
    ) where


-- | A class of types that have additional invariants defined upon them
-- that aren't enforced by the type system
class Validity a where
    isValid :: a -> Bool

-- | Any @Foldable@ of things that can be checked for validity can be checked
-- for validity
--
-- This includes lists, which means that the empty list is considered valid.
-- If the empty list should not be considered valid as part of your custom data
-- type, make sure to write a custom @Validity instance@
--
-- This also includes @Maybe@:
-- It makes sense to assume that 'Nothing' is valid.
-- If Nothing wasn't valid, you wouldn't have used a Maybe
-- in the datastructure.
instance (Validity a, Foldable t) => Validity (t a) where
    isValid = all isValid

