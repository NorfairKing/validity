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
    https://hackage.haskell.org/package/genvalidity ):

    > instance Validity Prime where
    >     isValid (Prime n) = isPrime n
    -}

module Data.Validity
    ( Validity(..)
    , constructValid
    , constructValidUnsafe
    ) where

import Data.Maybe (fromMaybe)


-- | A class of types that have additional invariants defined upon them
-- that aren't enforced by the type system
class Validity a where
    -- | Check whether a given value is a valid value.
    isValid :: a -> Bool

-- | Any tuple of things is valid if both of its elements are valid
instance (Validity a, Validity b) => Validity (a, b) where
    isValid (a, b) = isValid a && isValid b

-- | Any tuple of things is valid if all three of its elements are valid
instance (Validity a, Validity b, Validity c) => Validity (a, b, c) where
    isValid (a, b, c) = isValid a && isValid b && isValid c

-- | A list of things is valid if all of the things are valid.
--
-- This means that the empty list is considered valid.
-- If the empty list should not be considered valid as part of your custom data
-- type, make sure to write a custom @Validity instance@
instance Validity a => Validity [a] where
    isValid = all isValid

-- | A Maybe thing is valid if the thing inside is valid or it's nothing
-- It makes sense to assume that 'Nothing' is valid.
-- If Nothing wasn't valid, you wouldn't have used a Maybe
-- in the datastructure.
instance Validity a => Validity (Maybe a) where
    isValid Nothing = True
    isValid (Just a) = isValid a

-- | Trivially valid
instance Validity () where
    isValid = const True

-- | Trivially valid
instance Validity Bool where
    isValid = const True

-- | Trivially valid
instance Validity Ordering where
    isValid = const True

-- | Trivially valid
instance Validity Char where
    isValid = const True

-- | Trivially valid
instance Validity Int where
    isValid = const True

-- | Trivially valid
instance Validity Word where
    isValid = const True

-- | NOT trivially valid:
--
-- * NaN is not valid.
-- * Infinite values are not valid.
instance Validity Float where
    isValid d
      =  not (isNaN d)
      && not (isInfinite d)

-- | NOT trivially valid:
--
-- * NaN is not valid.
-- * Infinite values are not valid.
instance Validity Double where
    isValid d
      =  not (isNaN d)
      && not (isInfinite d)

-- | Trivially valid
instance Validity Integer where
    isValid = const True

-- | Construct a valid element from an unchecked element
constructValid :: Validity a => a -> Maybe a
constructValid p = if isValid p then Just p else Nothing

-- | Construct a valid element from an unchecked element, throwing 'error'
-- on invalid elements.
constructValidUnsafe :: (Show a, Validity a) => a -> a
constructValidUnsafe p = fromMaybe (error $ show p ++ " is not valid") $ constructValid p

