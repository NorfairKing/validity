{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}

{-|

    @Validity@ is used to specify additional invariants upon values that are not
    enforced by the type system.

    Let's take an example.
    Suppose we were to implement a type @Prime@ that represents prime integers.

    If you were to completely enforce the invariant that the represented number is
    a prime, then we could use 'Natural' and only store the index of the
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

    If certain typeclass invariants exist, you can make these explicit in the
    validity instance as well.
    For example, 'Fixed a' is only valid if 'a' has an 'HasResolution' instance,
    so the correct validity instance is @HasResolution a => Validity (Fixed a)@.
    -}
module Data.Validity
    ( Validity(..)
    , isInvalid
    , constructValid
    , constructValidUnsafe
    ) where

import Data.Fixed (Fixed(MkFixed), HasResolution)
import Data.Maybe (Maybe, fromMaybe)
import Data.Word (Word, Word8, Word16)
import GHC.Generics
#if MIN_VERSION_base(4,8,0)
import GHC.Natural (Natural, isValidNatural)
#endif
import GHC.Real (Ratio(..))

-- | A class of types that have additional invariants defined upon them
-- that aren't enforced by the type system
--
-- === Semantics for 'isValid'
--
-- 'isValid' should be an underapproximation of actual validity.
--
-- This means that if 'isValid' is not a perfect representation of actual
-- validity, for safety reasons, it should never return 'True' for invalid
-- values, but it may return 'False' for valid values.
--
-- For example:
--
-- > isValid = const False
--
-- is a valid implementation for any type, because it never returns 'True'
-- for invalid values.
--
-- > isValid (Even i) = i == 2
--
-- is a valid implementation for @newtype Even = Even Int@, but
--
-- > isValid (Even i) = even i || i == 1
--
-- is not because it returns 'True' for an invalid value: '1'.
--
-- === Automatic instances with 'Generic'
--
-- An instance of this class can be made automatically if the type in question
-- has a 'Generic' instance. This instance will try to use 'isValid' to
-- on all structural sub-parts of the value that is being checked for validity.
--
-- Example:
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > data MyType = MyType Double String
-- >     deriving (Show, Eq, Generic)
-- >
-- > instance Validity MyType
--
-- generates something like:
--
-- > instance Validity MyType where
-- >     isValid (MyType d s) = isValid d && isValid s
class Validity a where
    isValid :: a -> Bool
    default isValid :: (Generic a, GValidity (Rep a)) =>
        a -> Bool
    isValid = gIsValid . from

isInvalid :: Validity a => a -> Bool
isInvalid = not . isValid

-- | Any tuple of things is valid if both of its elements are valid
instance (Validity a, Validity b) => Validity (a, b) where
    isValid (a, b) = isValid a && isValid b

-- | Any Either of things is valid if the contents are valid in either of the cases.
instance (Validity a, Validity b) => Validity (Either a b) where
    isValid (Left a) = isValid a
    isValid (Right b) = isValid b

-- | Any triple of things is valid if all three of its elements are valid
instance (Validity a, Validity b, Validity c) => Validity (a, b, c) where
    isValid (a, b, c) = isValid a && isValid b && isValid c

-- | Any quadruple of things is valid if all four of its elements are valid
instance (Validity a, Validity b, Validity c, Validity d) =>
         Validity (a, b, c, d) where
    isValid (a, b, c, d) = isValid a && isValid b && isValid c && isValid d

-- | Any quintuple of things is valid if all five of its elements are valid
instance (Validity a, Validity b, Validity c, Validity d, Validity e) =>
         Validity (a, b, c, d, e) where
    isValid (a, b, c, d, e) =
        isValid a && isValid b && isValid c && isValid d && isValid e

-- | Any sextuple of things is valid if all six of its elements are valid
instance ( Validity a
         , Validity b
         , Validity c
         , Validity d
         , Validity e
         , Validity f
         ) =>
         Validity (a, b, c, d, e, f) where
    isValid (a, b, c, d, e, f) =
        isValid a &&
        isValid b && isValid c && isValid d && isValid e && isValid f

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

-- | Trivially valid
instance Validity Word8 where
    isValid = const True

-- | Trivially valid
instance Validity Word16 where
    isValid = const True

-- | NOT trivially valid:
--
-- * NaN is not valid.
-- * Infinite values are not valid.
instance Validity Float where
    isValid d = not (isNaN d) && not (isInfinite d)

-- | NOT trivially valid:
--
-- * NaN is not valid.
-- * Infinite values are not valid.
instance Validity Double where
    isValid d = not (isNaN d) && not (isInfinite d)

-- | Trivially valid
--
-- Integer is not trivially valid under the hood, but instantiating
-- 'Validity' correctly would force validity to depend on a specific
-- (big integer library @integer-gmp@ versus @integer-simple@).
-- This is rather impractical so for the time being we have opted for
-- assuming that an 'Integer' is always valid.
-- Even though this is not technically sound, it is good enough for now.
instance Validity Integer where
    isValid = const True
#if MIN_VERSION_base(4,8,0)
-- | Valid according to 'isValidNatural'
--
-- Only available with @base >= 4.8@.
instance Validity Natural where
    isValid = isValidNatural
#endif
-- | Valid if the contained 'Integer's are valid and the denominator is
-- strictly positive.
instance Validity Rational where
    isValid (d :% n) = isValid n && isValid d && d > 0

-- | Valid according to the contained 'Integer'.
instance HasResolution a => Validity (Fixed a) where
    isValid (MkFixed i) = isValid i

-- | Construct a valid element from an unchecked element
constructValid :: Validity a => a -> Maybe a
constructValid p =
    if isValid p
        then Just p
        else Nothing

-- | Construct a valid element from an unchecked element, throwing 'error'
-- on invalid elements.
constructValidUnsafe :: (Show a, Validity a) => a -> a
constructValidUnsafe p =
    fromMaybe (error $ show p ++ " is not valid") $ constructValid p

class GValidity f where
    gIsValid :: f a -> Bool

instance GValidity U1 where
    gIsValid U1 = True

instance (GValidity a, GValidity b) => GValidity (a :*: b) where
    gIsValid (a :*: b) = gIsValid a && gIsValid b

instance (GValidity a, GValidity b) => GValidity (a :+: b) where
    gIsValid (L1 x) = gIsValid x
    gIsValid (R1 x) = gIsValid x

instance (GValidity a) => GValidity (M1 i c a) where
    gIsValid (M1 x) = gIsValid x

instance (Validity a) => GValidity (K1 i a) where
    gIsValid (K1 x) = isValid x
