{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

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
    >     validate (Prime n) = isPrime n <?@> "The 'Int' is prime."
    >     isValid (Prime n) = isPrime n

    If certain typeclass invariants exist, you can make these explicit in the
    validity instance as well.
    For example, 'Fixed a' is only valid if 'a' has an 'HasResolution' instance,
    so the correct validity instance is @HasResolution a => Validity (Fixed a)@.
    -}
module Data.Validity
    ( Validity(..)
    -- * Helper functions to define 'isValid'
    , triviallyValid
    -- * Helper functions to define 'validate'
    , trivialValidation
    , isValidByValidating
    , check
    , (<?!>)
    , annotate
    , (<?@>)
    , validateByChecking
    , validateByCheckingName
    , validateByCheckingDefault
    -- * Utilities
    -- ** Utilities for validity checking
    , isInvalid
    , constructValid
    , constructValidUnsafe
    -- ** Utilities for validation
    , Validation(..)
    , ValidationChain(..)
    , checkValidity
    , prettyValidation
    -- * Re-exports
    , Monoid(..)
    ) where

import Data.Either (isRight)
import Data.Fixed (Fixed(MkFixed), HasResolution)
import Data.List (intercalate)
#if MIN_VERSION_base(4,9,0)
import Data.List.NonEmpty (NonEmpty((:|)))
#endif
import Data.Maybe (Maybe, fromMaybe)
#if MIN_VERSION_base(4,8,0)
#else
import Data.Monoid
import Data.Ratio
#endif
import Data.Word (Word, Word16, Word32, Word64, Word8)
import GHC.Generics
#if MIN_VERSION_base(4,8,0)
import GHC.Natural (Natural, isValidNatural)
#endif
import GHC.Real (Ratio(..))

-- | A class of types that have additional invariants defined upon them
-- that aren't enforced by the type system
--
-- === Purpose
--
-- 'validate' checks whether a given value is a valid value and reports all
-- reasons why the given value is not valid if that is the case.
--
-- 'isValid' only checks whether a given value is a valid value of its type.
--
-- === Instantiating 'Validity'
--
-- To instantiate 'Validity', one has to implement both 'isValid' and
-- 'validate'.
-- Start by implementing 'validate'. Use the helper functions below to define
-- all the reasons why a given value would be a valid value of its type.
-- Then define `isValid = isValidbyValidating' for now.
--
-- Example:
--
-- > newtype Even = Even Int
-- >
-- > instance Validity Even
-- >     validate (Event i)
-- >       even i <?@> "The contained 'Int' is even."
-- >     isValid = isValidByValidating
--
-- If it turns out that, at this point, 'isValid' is too slow for your taste,
-- you can replace the implementation of 'isValid' by a custom implementation.
-- However, it is important that this 'isValid' implementation has exactly
-- the same semantics as 'isValidbyValidating'.
--
-- Example:
--
-- > newtype Even = Even Int
-- >
-- > instance Validity Even
-- >     validate (Event i)
-- >       even i <?@> "The contained 'Int' is even."
-- >     isValid (Event i) = even i
--
-- === Semantics
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
-- > data MyType = MyType
-- >     { myDouble :: Double
-- >     { myString :: String
-- >     } deriving (Show, Eq, Generic)
-- >
-- > instance Validity MyType
--
-- generates something like:
--
-- > instance Validity MyType where
-- >     isValid (MyType d s)
-- >         = isValid d && isValid s
-- >     validate (MyType d s)
-- >         = d <?!> "myDouble"
-- >        <> s <?!> "myString"
class Validity a where
    validate :: a -> Validation
    default validate :: (Generic a, GValidity (Rep a)) =>
        a -> Validation
    validate = gValidate . from
    isValid :: a -> Bool
    default isValid :: (Generic a, GValidity (Rep a)) =>
        a -> Bool
    isValid = gIsValid . from

data ValidationChain
    = Violated String
    | Location String
               ValidationChain
    deriving (Show, Eq, Generic)

instance Validity ValidationChain

newtype Validation = Validation
    { unValidation :: [ValidationChain]
    } deriving (Show, Eq, Generic)

instance Validity Validation

instance Monoid Validation where
    mempty = Validation []
    mappend (Validation v1) (Validation v2) = Validation $ v1 ++ v2

-- | Declare any value to be valid.
--
-- > triviallyValid a = seq a True
triviallyValid :: a -> Bool
triviallyValid a = seq a True

-- | Implement 'isValid' by using 'validate' and checking that there are no
-- reasons that the value is invalid.
isValidByValidating :: Validity a => a -> Bool
isValidByValidating = isRight . checkValidity

-- | Declare any value to be valid in validation
--
-- > trivialValidation a = seq a mempty
trivialValidation :: a -> Validation
trivialValidation a = seq a mempty

-- | Implement 'validate' by using 'isValid' and using the given string as the
-- reason if the value is invalid.
validateByChecking :: Validity a => String -> a -> Validation
validateByChecking s a = isValid a <?@> s

-- | Implement 'validate' by using 'isValid' and using the given name to define
-- the reason if the value is invalid.
--
-- > validateByCheckingName name = validateByChecking $ unwords ["The", name, "valid."]
validateByCheckingName :: Validity a => String -> a -> Validation
validateByCheckingName name =
    validateByChecking $ unwords ["The", name, "valid."]

-- | Implement 'validate' by using 'isValid' and using a default reason if the
-- value is invalid.
--
-- > validateByCheckingDefault = validateByChecking "The value is valid."
validateByCheckingDefault :: Validity a => a -> Validation
validateByCheckingDefault = validateByChecking "The value is valid."

-- | Check that a given invariant holds.
--
-- The given string should describe the invariant, not the violation.
--
-- Example:
--
-- > check (x < 5) "x is strictly smaller than 5"
--
-- instead of
--
-- > check (x < 5) "x is greater than 5"
check :: Bool -> String -> Validation
check b err =
    if b
        then mempty
        else Validation [Violated err]

-- | Infix operator for 'check'
--
-- Example:
--
-- > x < 5 <?@> "x is strictly smaller than 5"
(<?@>) :: Bool -> String -> Validation
(<?@>) = check

infixr 0 <?@>

-- | Declare a sub-part as a necessary part for validation, and annotate it with a name.
--
-- Example:
--
-- > validate (a, b) =
-- >     mconcat
-- >         [ annotate a "The first element of the tuple"
-- >         , annotate b "The second element of the tuple"
-- >         ]
annotate :: Validity a => a -> String -> Validation
annotate = annotateValidation . validate

-- | Infix operator for 'annotate'
--
-- Example:
--
-- > validate (a, b) =
-- >     mconcat
-- >         [ a <?!> "The first element of the tuple"
-- >         , b <?!> "The second element of the tuple"
-- >         ]
(<?!>) :: Validity a => a -> String -> Validation
(<?!>) = annotate

infixr 0 <?!>

-- | Any tuple of things is valid if both of its elements are valid
instance (Validity a, Validity b) => Validity (a, b) where
    isValid (a, b) = isValid a && isValid b
    validate (a, b) =
        mconcat
            [ a <?!> "The first element of the tuple"
            , b <?!> "The second element of the tuple"
            ]

-- | Any Either of things is valid if the contents are valid in either of the cases.
instance (Validity a, Validity b) => Validity (Either a b) where
    isValid (Left a) = isValid a
    isValid (Right b) = isValid b
    validate (Left a) = a <?!> "The 'Left'"
    validate (Right b) = b <?!> "The 'Right'"

-- | Any triple of things is valid if all three of its elements are valid
instance (Validity a, Validity b, Validity c) => Validity (a, b, c) where
    isValid (a, b, c) = isValid a && isValid b && isValid c
    validate (a, b, c) =
        mconcat
            [ a <?!> "The first element of the triple"
            , b <?!> "The second element of the triple"
            , c <?!> "The third element of the triple"
            ]

-- | Any quadruple of things is valid if all four of its elements are valid
instance (Validity a, Validity b, Validity c, Validity d) =>
         Validity (a, b, c, d) where
    isValid (a, b, c, d) = isValid a && isValid b && isValid c && isValid d
    validate (a, b, c, d) =
        mconcat
            [ a <?!> "The first element of the quadruple"
            , b <?!> "The second element of the quadruple"
            , c <?!> "The third element of the quadruple"
            , d <?!> "The fourth element of the quadruple"
            ]

-- | Any quintuple of things is valid if all five of its elements are valid
instance (Validity a, Validity b, Validity c, Validity d, Validity e) =>
         Validity (a, b, c, d, e) where
    isValid (a, b, c, d, e) =
        isValid a && isValid b && isValid c && isValid d && isValid e
    validate (a, b, c, d, e) =
        mconcat
            [ a <?!> "The first element of the quintuple"
            , b <?!> "The second element of the quintuple"
            , c <?!> "The third element of the quintuple"
            , d <?!> "The fourth element of the quintuple"
            , e <?!> "The fifth element of the quintuple"
            ]

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
    validate (a, b, c, d, e, f) =
        mconcat
            [ a <?!> "The first element of the sextuple"
            , b <?!> "The second element of the sextuple"
            , c <?!> "The third element of the sextuple"
            , d <?!> "The fourth element of the sextuple"
            , e <?!> "The fifth element of the sextuple"
            , f <?!> "The sixth element of the sextuple"
            ]

-- | A list of things is valid if all of the things are valid.
--
-- This means that the empty list is considered valid.
-- If the empty list should not be considered valid as part of your custom data
-- type, make sure to write a custom @Validity instance@
instance Validity a => Validity [a] where
    isValid = all isValid
    validate =
        mconcat .
        map
            (\(ix, e) ->
                 e <?!>
                 unwords
                     [ "The element at index"
                     , show (ix :: Integer)
                     , "in the list"
                     ]) .
        zip [0 ..]
#if MIN_VERSION_base(4,9,0)
-- | A nonempty list is valid if all the elements are valid.
--
-- See the instance for 'Validity [a]' for more information.
instance Validity a => Validity (NonEmpty a) where
    isValid = all isValid
    validate (e :| es) =
        mconcat
            [ e <?!> "The first element of the nonempty list"
            , es <?!> "The rest of the elements of the nonempty list"
            ]
#endif
-- | A Maybe thing is valid if the thing inside is valid or it's nothing
-- It makes sense to assume that 'Nothing' is valid.
-- If Nothing wasn't valid, you wouldn't have used a Maybe
-- in the datastructure.
instance Validity a => Validity (Maybe a) where
    isValid Nothing = True
    isValid (Just a) = isValid a
    validate Nothing = mempty
    validate (Just a) = a <?!> "The 'Just'"

-- | Trivially valid
instance Validity () where
    isValid = triviallyValid
    validate = validateByCheckingName "()"

-- | Trivially valid
instance Validity Bool where
    isValid = triviallyValid
    validate = validateByCheckingName "Bool"

-- | Trivially valid
instance Validity Ordering where
    isValid = triviallyValid
    validate = validateByCheckingName "Ordering"

-- | Trivially valid
instance Validity Char where
    isValid = triviallyValid
    validate = validateByCheckingName "Char"

-- | Trivially valid
instance Validity Int where
    isValid = triviallyValid
    validate = validateByCheckingName "Int"

-- | Trivially valid
instance Validity Word where
    isValid = triviallyValid
    validate = validateByCheckingName "Word"

-- | Trivially valid
instance Validity Word8 where
    isValid = triviallyValid
    validate = validateByCheckingName "Word8"

-- | Trivially valid
instance Validity Word16 where
    isValid = triviallyValid
    validate = validateByCheckingName "Word16"

-- | Trivially valid
instance Validity Word32 where
    isValid = triviallyValid
    validate = validateByCheckingName "Word34"

-- | Trivially valid
instance Validity Word64 where
    isValid = triviallyValid
    validate = validateByCheckingName "Word64"

-- | NOT trivially valid:
--
-- * NaN is not valid.
-- * Infinite values are not valid.
instance Validity Float where
    isValid f = not (isNaN f) && not (isInfinite f)
    validate f =
        mconcat
            [ not (isNaN f) <?@> "The Float is not Nan."
            , not (isInfinite f) <?@> "The Float is not infinite."
            ]

-- | NOT trivially valid:
--
-- * NaN is not valid.
-- * Infinite values are not valid.
instance Validity Double where
    isValid d = not (isNaN d) && not (isInfinite d)
    validate d =
        mconcat
            [ not (isNaN d) <?@> "The Double is not NaN."
            , not (isInfinite d) <?@> "The Double is not infinite."
            ]

-- | Trivially valid
--
-- Integer is not trivially valid under the hood, but instantiating
-- 'Validity' correctly would force validity to depend on a specific
-- (big integer library @integer-gmp@ versus @integer-simple@).
-- This is rather impractical so for the time being we have opted for
-- assuming that an 'Integer' is always valid.
-- Even though this is not technically sound, it is good enough for now.
instance Validity Integer where
    isValid = triviallyValid
    validate = validateByCheckingName "Integer"
#if MIN_VERSION_base(4,8,0)
-- | Valid according to 'isValidNatural'
--
-- Only available with @base >= 4.8@.
instance Validity Natural where
    isValid = isValidNatural
    validate = validateByChecking "Natural"
#endif
-- | Valid if the contained numbers are valid and the denominator is
-- strictly positive.
instance (Num a, Ord a, Validity a) => Validity (Ratio a) where
    isValid (n :% d) = isValid n && isValid d && d > 0
    validate (n :% d) =
        mconcat
            [ n <?!> "The numerator"
            , d <?!> "The denominator"
            , (d > 0) <?@> "The denominator is strictly positive."
            ]

-- | Valid according to the contained 'Integer'.
instance HasResolution a => Validity (Fixed a) where
    isValid (MkFixed i) = isValid i
    validate (MkFixed i) = validate i

annotateValidation :: Validation -> String -> Validation
annotateValidation val s =
    case val of
        Validation errs -> Validation $ map (Location s) errs

class GValidity f where
    gIsValid :: f a -> Bool
    gValidate :: f a -> Validation

instance GValidity U1 where
    gIsValid = triviallyValid
    gValidate = trivialValidation

instance GValidity V1 where
    gIsValid = triviallyValid
    gValidate = trivialValidation

instance (GValidity a, GValidity b) => GValidity (a :*: b) where
    gIsValid (a :*: b) = gIsValid a && gIsValid b
    gValidate (a :*: b) = gValidate a `mappend` gValidate b

instance (GValidity a, GValidity b) => GValidity (a :+: b) where
    gIsValid (L1 x) = gIsValid x
    gIsValid (R1 x) = gIsValid x
    gValidate (L1 x) = gValidate x
    gValidate (R1 x) = gValidate x

instance (GValidity a, Datatype c) => GValidity (M1 D c a) where
    gIsValid (M1 x) = gIsValid x
    gValidate m1 = gValidate (unM1 m1)

instance (GValidity a, Constructor c) => GValidity (M1 C c a) where
    gIsValid (M1 x) = gIsValid x
    gValidate m1 = gValidate (unM1 m1) `annotateValidation` conName m1

instance (GValidity a, Selector c) => GValidity (M1 S c a) where
    gIsValid (M1 x) = gIsValid x
    gValidate m1 = gValidate (unM1 m1) `annotateValidation` selName m1

instance (Validity a) => GValidity (K1 R a) where
    gIsValid (K1 x) = isValid x
    gValidate (K1 x) = validate x

-- | Check whether 'isInvalid' is not valid.
--
-- > isInvalid = not . isValid
isInvalid :: Validity a => a -> Bool
isInvalid = not . isValid

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

-- | validate a given value.
--
-- This function returns either all the reasons why the given value is invalid,
-- in the form of a list of 'ValidationChain's, or it returns 'Right' with the
-- input value, as evidence that it is valid.
--
-- Note: You map want to use 'prettyValidation' instead, if you want to
-- display these 'ValidationChain's to a user.
checkValidity :: Validity a => a -> Either [ValidationChain] a
checkValidity a =
    case validate a of
        Validation [] -> Right a
        Validation errs -> Left errs

-- | validate a given value, and return a nice error if the value is invalid.
prettyValidation :: Validity a => a -> Either String a
prettyValidation a =
    case checkValidity a of
        Right a_ -> Right a_
        Left errs -> Left $ intercalate "\n" $ map (errCascade . toStrings) errs
  where
    toStrings (Violated s) = ["Violated: " ++ s]
    toStrings (Location s vc) = s : toStrings vc
    errCascade errList =
        intercalate "\n" $
        flip map (zip [0 ..] errList) $ \(i, segment) ->
            case i of
                0 -> segment
                _ -> replicate i ' ' ++ "\\ " ++ segment
