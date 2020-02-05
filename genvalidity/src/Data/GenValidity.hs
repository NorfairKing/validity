{-|

    @GenValidity@ exists to make tests involving @Validity@ types easier and speed
    up the generation of data for them.

    Let's use the example from @Data.Validity@ again: A datatype that represents
    primes.
    To implement tests for this datatype, we would have to be able to generate
    both primes and non-primes. We could do this with
    @(Prime <$> arbitrary) `suchThat` isValid@
    but this is tedious and inefficient.

    The @GenValid@ type class allows you to specify how to (efficiently)
    generate valid data of the given type to allow for easier and quicker testing.
    Just instantiating @GenUnchecked@ already gives you access to a default instance
    of @GenValid@ and @GenInvalid@ but writing custom implementations of these functions
    may speed up the generation of data.

    For example, to generate primes, we don't have to consider even numbers other
    than 2. A more efficient implementation could then look as follows:

    > instance GenUnchecked Prime where
    >     genUnchecked = Prime <$> arbitrary

    > instance GenValid Prime where
    >     genValid = Prime <$>
    >        (oneof
    >          [ pure 2
    >          , ((\y -> 2 * abs y + 1) <$> arbitrary) `suchThat` isPrime)
    >          ])


    Typical examples of tests involving validity could look as follows:

    > it "succeeds when given valid input" $ do
    >     forAllValid $ \input ->
    >         myFunction input `shouldSatisfy` isRight

    > it "produces valid output when it succeeds" $ do
    >     forAllUnchecked $ \input ->
    >         case myFunction input of
    >             Nothing -> return () -- Can happen
    >             Just output -> output `shouldSatisfy` isValid

    Definitely also look at the genvalidity-property and genvalidity-hspec packages
    for more info on how to use this package.
    -}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 710
#define OVERLAPPING_ {-# OVERLAPPING #-}
#else
{-# LANGUAGE OverlappingInstances  #-}
#define OVERLAPPING_
#endif
#if MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Data.GenValidity
    ( GenUnchecked(..)
    , GenValid(..)
    , GenInvalid(..)

    -- * Helper functions
    , genValidStructurally
    , genValidStructurallyWithoutExtraChecking
    , shrinkValidStructurally
    , shrinkValidStructurallyWithoutExtraFiltering
    , module Data.GenValidity.Utils

    -- * Strange, possibly useful functions
    , genUtf16SurrogateCodePoint

    -- * Re-exports
    , module Data.Validity

    -- * The Generics magic
    , genericGenUnchecked
    , GGenUnchecked(..)
    , genericShrinkUnchecked
    , uncheckedRecursivelyShrink
    , GUncheckedRecursivelyShrink(..)
    , uncheckedSubterms
    , GUncheckedSubterms(..)
    , GUncheckedSubtermsIncl(..)
    , GGenValid(..)
    , GValidRecursivelyShrink(..)
    , structurallyValidSubterms
    , GValidSubterms(..)
    , GValidSubtermsIncl(..)
    ) where

import Data.Validity

import Data.Fixed (Fixed(..), HasResolution)
#if MIN_VERSION_base(4,9,0)
import Data.List.NonEmpty (NonEmpty((:|)))
#endif
#if MIN_VERSION_base(4,8,0)
import Data.Word (Word8, Word16, Word32, Word64)
#else
import Data.Word (Word, Word8, Word16, Word32, Word64)
#endif
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Char (chr)
import Data.Ratio ((%))
import GHC.Generics
import GHC.Real (Ratio(..))

import Test.QuickCheck hiding (Fixed)

#if MIN_VERSION_base(4,8,0)
import GHC.Natural
#else
import Control.Applicative ((<*>), (<$>), pure)
#endif

import Data.GenValidity.Utils

{-# ANN module "HLint: ignore Reduce duplication" #-}

-- | A class of types for which truly arbitrary values can be generated.
--
-- === How to instantiate 'GenUnchecked'
--
-- __Step 1__: Try to instantiate 'GenUnchecked' via 'Generic'.
--         __this is probably what you want__
--
-- An instance of this class can be made automatically if the type in question
-- has a 'Generic' instance. This instance will try to use 'genUnchecked' to
-- generate all structural sub-parts of the value that is being generated.
--
-- Example:
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > data MyType = MyType Rational String
-- >     deriving (Show, Eq, Generic)
-- >
-- > instance GenUnchecked MyType
--
-- generates something like:
--
-- > instance GenUnchecked MyType where
-- >     genUnchecked = MyType <$> genUnchecked <*> genUnchecked
--
-- If this is not possible because there is no 'GenUnchecked' instance available for one of the
-- sub-parts of your type, __then do not instantiate 'GenUnchecked' for your type__.
-- Just continue with 'GenValid' instead.
--
-- __Step 2__: If an instatiation via 'Generic' is not possible, then you should emulate what
--         'genericGenUnchecked' does.
--         This means that all sub-parts should be  generated using 'genUnchecked'.
--         Make sure to generate any possible value, valid or not, that can exist at runtime
--         even when taking the existence of 'Unsafe.Coerce.unsafeCoerce' into account.
--
-- === Warning: Invalid values can be funky
--
-- Some types have serious validity constraints. See 'Rational' for example.
-- These can behave very strangely when they are not valid.
-- In that case, __do not override 'GenUnchecked' such that 'genUnchecked' only generates valid values__.
-- In that case, do not override 'genUnchecked' at all.
-- Instead, use 'genValid' from 'GenValid' (see below) instead and consider not instantiating 'GenUnchecked' at all.
class GenUnchecked a where
    genUnchecked :: Gen a
    default genUnchecked :: (Generic a, GGenUnchecked (Rep a)) =>
        Gen a
    genUnchecked = genericGenUnchecked

    shrinkUnchecked :: a -> [a]
    default shrinkUnchecked ::
        (Generic a, GUncheckedRecursivelyShrink (Rep a), GUncheckedSubterms (Rep a) a) =>
        a -> [a]
    shrinkUnchecked = genericShrinkUnchecked

-- | A class of types for which valid values can be generated.
--
-- === How to instantiate 'GenValid'
--
-- __Step 1__: Try to instantiate 'GenValid' without overriding any functions.
--             This is only possible if your type has a 'GenUnchecked' instance.
--             If it doesn't, go to step 2.
--             It is possible that, if few values are valid or if validity
--             checking is expensive, that the resulting generator is too slow.
--             In that case, go to Step 2.
--
-- __Step 2__: Try to instantiate 'GenValid' using the helper functions via 'Generic'
--             This involves using 'genValidStructurally' to override 'genValid' and
--             using 'shrinkValidStructurally' to override 'shrinkValid'.
--             __Every time you override 'genValid', you should also override 'shrinkValid'__
--
-- __Step 3__: If the above is not possible due to lack of a 'Generic' instance,
--             then you should emulate what 'genValidStructurally' does.
--             This means that all sub-parts should be generated using 'genValid'.
--             Make sure to generate any possible valid value, but only valid values.
--
-- === A note about 'Arbitrary'
--
-- If you also write @Arbitrary@ instances for @GenValid@ types, it may be
-- best to simply use
--
-- > arbitrary = genValid
-- > shrink = shrinkValid
class Validity a => GenValid a where
    -- | Generate a valid datum, this should cover all possible valid values in
    -- the type
    --
    -- The default implementation is as follows:
    --
    -- >  genValid = genUnchecked `suchThat` isValid
    --
    -- To speed up testing, it may be a good idea to implement this yourself.
    -- If you do, make sure that it is possible to generate all possible valid
    -- data, otherwise your testing may not cover all cases.
    genValid :: Gen a
    default genValid :: GenUnchecked a => Gen a
    genValid = genUnchecked `suchThat` isValid

    -- | Shrink a valid value.
    --
    -- The default implementation is as follows:
    --
    -- >  shrinkValid = filter isValid . shrinkUnchecked
    --
    -- It is important that this shrinking function only shrinks values to valid values.
    -- If `shrinkValid` ever shrinks a value to an invalid value, the test that is being shrunk for
    -- might fail for a different reason than for the reason that it originally failed.
    -- This would lead to very confusing error messages.
    shrinkValid :: a -> [a]
    default shrinkValid :: GenUnchecked a => a -> [a]
    shrinkValid = filter isValid . shrinkUnchecked

-- | A class of types for which invalid values can be generated.
--
-- === How to instantiate 'GenInvalid'
--
-- __Step 1__: Realise that you probably do not want to.
--             It makes no sense, and serves no purpose, to instantiate 'GenInvalid' for types
--             which contain no invalid values. (In fact, the default implementation will go into
--             an infinite loop for such types.)
--             You should only instantiate 'GenInvalid' if you explicitly want to use it
--             to write tests that deal with invalid values, or if you are writing a container
--             for parametric values.
--
-- __Step 2__: Instantiate 'GenInvalid' without overriding any functions.
class Validity a => GenInvalid a where
    genInvalid :: Gen a
    -- | Generate an invalid datum, this should cover all possible invalid
    -- values
    --
    -- > genInvalid = genUnchecked `suchThat` isInvalid
    --
    -- To speed up testing, it may be a good idea to implement this yourself.
    -- If you do, make sure that it is possible to generate all possible
    -- invalid data, otherwise your testing may not cover all cases.
    default genInvalid :: GenUnchecked a => Gen a
    genInvalid = genUnchecked `suchThat` isInvalid

    shrinkInvalid :: a -> [a]
    default shrinkInvalid :: GenUnchecked a => a -> [a]
    shrinkInvalid = filter isInvalid . shrinkUnchecked

instance (GenUnchecked a, GenUnchecked b) => GenUnchecked (a, b) where
    genUnchecked =
        sized $ \n -> do
            (r, s) <- genSplit n
            a <- resize r genUnchecked
            b <- resize s genUnchecked
            return (a, b)
    shrinkUnchecked (a, b) = ((,) <$> shrinkUnchecked a <*> shrinkUnchecked b)
      ++ [ (a', b) | a' <- shrinkUnchecked a ]
      ++ [ (a, b') | b' <- shrinkUnchecked b ]

instance (GenValid a, GenValid b) => GenValid (a, b) where
    genValid =
        sized $ \n -> do
            (r, s) <- genSplit n
            a <- resize r genValid
            b <- resize s genValid
            return (a, b)
    shrinkValid = shrinkTuple shrinkValid shrinkValid

instance (GenUnchecked a, GenInvalid a, GenUnchecked b, GenInvalid b) => GenInvalid (a, b) where
    genInvalid =
        sized $ \n -> do
            (r, s) <- genSplit n
            oneof
                [ do a <- resize r genUnchecked
                     b <- resize s genInvalid
                     return (a, b)
                , do a <- resize r genInvalid
                     b <- resize s genUnchecked
                     return (a, b)
                ]

instance (GenUnchecked a, GenUnchecked b) => GenUnchecked (Either a b) where
    genUnchecked = oneof [Left <$> genUnchecked, Right <$> genUnchecked]
    shrinkUnchecked (Left a) = Left <$> shrinkUnchecked a
    shrinkUnchecked (Right b) = Right <$> shrinkUnchecked b

instance (GenValid a, GenValid b) => GenValid (Either a b) where
    genValid = oneof [Left <$> genValid, Right <$> genValid]
    shrinkValid (Left a) = Left <$> shrinkValid a
    shrinkValid (Right b) = Right <$> shrinkValid b

-- | This instance ensures that the generated tupse contains at least one invalid element. The other element is unchecked.
instance (GenInvalid a, GenInvalid b) => GenInvalid (Either a b) where
    genInvalid = oneof [Left <$> genInvalid, Right <$> genInvalid]
    shrinkInvalid (Left v) = Left <$> shrinkInvalid v
    shrinkInvalid (Right v) = Right <$> shrinkInvalid v

instance (GenUnchecked a, GenUnchecked b, GenUnchecked c) =>
         GenUnchecked (a, b, c) where
    genUnchecked =
        sized $ \n -> do
            (r, s, t) <- genSplit3 n
            a <- resize r genUnchecked
            b <- resize s genUnchecked
            c <- resize t genUnchecked
            return (a, b, c)
    shrinkUnchecked (a, b, c) =
        [ (a', b', c')
        | (a', (b', c')) <- shrinkUnchecked (a, (b, c))
        ]

instance (GenValid a, GenValid b, GenValid c) => GenValid (a, b, c) where
    genValid =
        sized $ \n -> do
            (r, s, t) <- genSplit3 n
            a <- resize r genValid
            b <- resize s genValid
            c <- resize t genValid
            return (a, b, c)
    shrinkValid (a, b, c) =
        [ (a', b', c')
        | (a', (b', c')) <- shrinkValid (a, (b, c))
        ]

-- | This instance ensures that the generated triple contains at least one invalid element. The other two are unchecked.
instance ( GenUnchecked a, GenUnchecked b, GenUnchecked c
         , GenInvalid a, GenInvalid b, GenInvalid c) =>
         GenInvalid (a, b, c) where
    genInvalid =
        sized $ \n -> do
            (r, s, t) <- genSplit3 n
            oneof
                [ do a <- resize r genInvalid
                     b <- resize s genUnchecked
                     c <- resize t genUnchecked
                     return (a, b, c)
                , do a <- resize r genUnchecked
                     b <- resize s genInvalid
                     c <- resize t genUnchecked
                     return (a, b, c)
                , do a <- resize r genUnchecked
                     b <- resize s genUnchecked
                     c <- resize t genInvalid
                     return (a, b, c)
                ]

instance (GenUnchecked a, GenUnchecked b, GenUnchecked c, GenUnchecked d) =>
         GenUnchecked (a, b, c, d) where
    genUnchecked =
        sized $ \n -> do
            (r, s, t, u) <- genSplit4 n
            a <- resize r genUnchecked
            b <- resize s genUnchecked
            c <- resize t genUnchecked
            d <- resize u genUnchecked
            return (a, b, c, d)
    shrinkUnchecked (a, b, c, d) =
        [ (a', b', c', d')
        | (a', (b', (c', d'))) <- shrinkUnchecked (a, (b, (c, d)))
        ]

instance (GenValid a, GenValid b, GenValid c, GenValid d) =>
         GenValid (a, b, c, d) where
    genValid =
        sized $ \n -> do
            (r, s, t, u) <- genSplit4 n
            a <- resize r genValid
            b <- resize s genValid
            c <- resize t genValid
            d <- resize u genValid
            return (a, b, c, d)
    shrinkValid (a, b, c, d) =
        [ (a', b', c', d')
        | (a', (b', (c', d'))) <- shrinkValid (a, (b, (c, d)))
        ]

-- | This instance ensures that the generated triple contains at least one invalid element. The other two are unchecked.
instance ( GenUnchecked a, GenUnchecked b, GenUnchecked c, GenUnchecked d
         , GenInvalid a, GenInvalid b, GenInvalid c, GenInvalid d) =>
         GenInvalid (a, b, c, d) where
    genInvalid =
        sized $ \n -> do
            (r, s, t, u) <- genSplit4 n
            oneof
                [ do a <- resize r genInvalid
                     b <- resize s genUnchecked
                     c <- resize t genUnchecked
                     d <- resize u genUnchecked
                     return (a, b, c, d)
                , do a <- resize r genUnchecked
                     b <- resize s genInvalid
                     c <- resize t genUnchecked
                     d <- resize u genUnchecked
                     return (a, b, c, d)
                , do a <- resize r genUnchecked
                     b <- resize s genUnchecked
                     c <- resize t genInvalid
                     d <- resize u genUnchecked
                     return (a, b, c, d)
                , do a <- resize r genUnchecked
                     b <- resize s genUnchecked
                     c <- resize t genUnchecked
                     d <- resize u genInvalid
                     return (a, b, c, d)
                ]

instance (GenUnchecked a, GenUnchecked b, GenUnchecked c, GenUnchecked d, GenUnchecked e) =>
         GenUnchecked (a, b, c, d, e) where
    genUnchecked =
        sized $ \n -> do
            (r, s, t, u, v) <- genSplit5 n
            a <- resize r genUnchecked
            b <- resize s genUnchecked
            c <- resize t genUnchecked
            d <- resize u genUnchecked
            e <- resize v genUnchecked
            return (a, b, c, d, e)
    shrinkUnchecked (a, b, c, d, e) =
        [ (a', b', c', d', e')
        | (a', (b', (c', (d', e')))) <- shrinkUnchecked (a, (b, (c, (d, e))))
        ]

instance (GenValid a, GenValid b, GenValid c, GenValid d, GenValid e) =>
         GenValid (a, b, c, d, e) where
    genValid =
        sized $ \n -> do
            (r, s, t, u, v) <- genSplit5 n
            a <- resize r genValid
            b <- resize s genValid
            c <- resize t genValid
            d <- resize u genValid
            e <- resize v genValid
            return (a, b, c, d, e)
    shrinkValid (a, b, c, d, e) =
        [ (a', b', c', d', e')
        | (a', (b', (c', (d', e')))) <- shrinkValid (a, (b, (c, (d, e))))
        ]

-- | This instance ensures that the generated triple contains at least one invalid element. The other two are unchecked.
instance ( GenUnchecked a, GenUnchecked b, GenUnchecked c, GenUnchecked d, GenUnchecked e
         , GenInvalid a, GenInvalid b, GenInvalid c, GenInvalid d, GenInvalid e) =>
         GenInvalid (a, b, c, d, e) where
    genInvalid =
        sized $ \n -> do
            (r, s, t, u, v) <- genSplit5 n
            oneof
                [ do a <- resize r genInvalid
                     b <- resize s genUnchecked
                     c <- resize t genUnchecked
                     d <- resize u genUnchecked
                     e <- resize v genUnchecked
                     return (a, b, c, d, e)
                , do a <- resize r genUnchecked
                     b <- resize s genInvalid
                     c <- resize t genUnchecked
                     d <- resize u genUnchecked
                     e <- resize v genUnchecked
                     return (a, b, c, d, e)
                , do a <- resize r genUnchecked
                     b <- resize s genUnchecked
                     c <- resize t genInvalid
                     d <- resize u genUnchecked
                     e <- resize v genUnchecked
                     return (a, b, c, d, e)
                , do a <- resize r genUnchecked
                     b <- resize s genUnchecked
                     c <- resize t genUnchecked
                     d <- resize u genInvalid
                     e <- resize v genUnchecked
                     return (a, b, c, d, e)
                , do a <- resize r genUnchecked
                     b <- resize s genUnchecked
                     c <- resize t genUnchecked
                     d <- resize u genUnchecked
                     e <- resize v genInvalid
                     return (a, b, c, d, e)
                ]

instance GenUnchecked a => GenUnchecked (Maybe a) where
    genUnchecked = oneof [pure Nothing, Just <$> genUnchecked]
    shrinkUnchecked Nothing = []
    shrinkUnchecked (Just a) = Nothing : (Just <$> shrinkUnchecked a)


instance GenValid a => GenValid (Maybe a) where
    genValid = oneof [pure Nothing, Just <$> genValid]
    shrinkValid Nothing = []
    shrinkValid (Just a) = Nothing : (Just <$> shrinkValid a)

instance GenInvalid a => GenInvalid (Maybe a) where
    genInvalid = Just <$> genInvalid
    shrinkInvalid Nothing = [] -- Should not happen
    shrinkInvalid (Just a) = Just <$> shrinkInvalid a

#if MIN_VERSION_base(4,9,0)
instance GenUnchecked a => GenUnchecked (NonEmpty a) where
    genUnchecked = genNonEmptyOf genUnchecked
    shrinkUnchecked (v :| vs) = [ e :| es | (e, es) <- shrinkUnchecked (v, vs)]

instance GenValid a => GenValid (NonEmpty a) where
    genValid = genNonEmptyOf genValid
    shrinkValid (v :| vs) = [ e :| es | (e, es) <- shrinkValid (v, vs)]

instance (GenUnchecked a, GenInvalid a) => GenInvalid (NonEmpty a) where
    genInvalid = genNonEmptyOf genInvalid
#endif

instance GenUnchecked a => GenUnchecked [a] where
    genUnchecked = genListOf genUnchecked
    shrinkUnchecked = shrinkList shrinkUnchecked

-- | If we can generate values of a certain type, we can also generate lists of
-- them.
instance GenValid a => GenValid [a] where
    genValid = genListOf genValid
    shrinkValid = shrinkList shrinkValid

-- | This instance ensures that the generated list contains at least one element
-- that satisfies 'isInvalid'. The rest is unchecked.
instance (GenUnchecked a, GenInvalid a) => GenInvalid [a] where
    genInvalid =
        sized $ \n -> do
            (x, y, z) <- genSplit3 n
            before <- resize x $ genListOf genUnchecked
            middle <- resize y genInvalid
            after <- resize z $ genListOf genUnchecked
            return $ before ++ [middle] ++ after

instance GenUnchecked () where
    genUnchecked = arbitrary
    shrinkUnchecked = shrink

instance GenValid () where
    genValid = genUnchecked
    shrinkValid = shrinkUnchecked

instance GenUnchecked Bool where
    genUnchecked = arbitrary
    shrinkUnchecked = shrink

instance GenValid Bool where
    genValid = genUnchecked
    shrinkValid = shrinkUnchecked

instance GenUnchecked Ordering where
    genUnchecked = arbitrary
    shrinkUnchecked = shrink

instance GenValid Ordering where
    genValid = genUnchecked
    shrinkValid = shrinkUnchecked

instance GenUnchecked Char where
    genUnchecked = frequency [(9, choose (minBound, maxBound)), (1, genUtf16SurrogateCodePoint)]
    shrinkUnchecked = shrink

genUtf16SurrogateCodePoint :: Gen Char
genUtf16SurrogateCodePoint = chr <$> oneof [choose (0xD800, 0xDBFF), choose (0xDC00, 0xDFFF)]

instance GenValid Char where
    genValid = genUnchecked
    shrinkValid = shrinkUnchecked

instance GenUnchecked Int where
    genUnchecked = genIntX
    shrinkUnchecked = shrink

instance GenValid Int where
    genValid = genUnchecked
    shrinkValid = shrinkUnchecked

instance GenUnchecked Int8 where
    genUnchecked = genIntX
    shrinkUnchecked = shrink

instance GenValid Int8 where
    genValid = genUnchecked
    shrinkValid = shrinkUnchecked

instance GenUnchecked Int16 where
    genUnchecked = genIntX
    shrinkUnchecked = shrink

instance GenValid Int16 where
    genValid = genUnchecked
    shrinkValid = shrinkUnchecked

instance GenUnchecked Int32 where
    genUnchecked = genIntX
    shrinkUnchecked = shrink

instance GenValid Int32 where
    genValid = genUnchecked
    shrinkValid = shrinkUnchecked

instance GenUnchecked Int64 where
    genUnchecked = genIntX
    shrinkUnchecked = shrink

instance GenValid Int64 where
    genValid = genUnchecked
    shrinkValid = shrinkUnchecked

instance GenUnchecked Word where
    genUnchecked = genWordX
    shrinkUnchecked = shrink

instance GenValid Word where
    genValid = genUnchecked
    shrinkValid = shrinkUnchecked

instance GenUnchecked Word8 where
    genUnchecked = genWordX
    shrinkUnchecked = shrink

instance GenValid Word8 where
    genValid = genUnchecked
    shrinkValid = shrinkUnchecked

instance GenUnchecked Word16 where
    genUnchecked = genWordX
    shrinkUnchecked = shrink

instance GenValid Word16 where
    genValid = genUnchecked
    shrinkValid = shrinkUnchecked

instance GenUnchecked Word32 where
    genUnchecked = genWordX
    shrinkUnchecked = shrink

instance GenValid Word32 where
    genValid = genUnchecked
    shrinkValid = shrinkUnchecked

instance GenUnchecked Word64 where
    genUnchecked = genWordX
    shrinkUnchecked = shrink

instance GenValid Word64 where
    genValid = genUnchecked
    shrinkValid = shrinkUnchecked

instance GenUnchecked Float where
    genUnchecked = genFloat
#if MIN_VERSION_QuickCheck(2,9,2)
    shrinkUnchecked f = if
      | isInfinite f -> []
      | isNaN f -> []
      | otherwise -> shrink f
#else
    shrinkUnchecked _ = []
#endif

instance GenValid Float where
    genValid = genUnchecked
    shrinkValid = shrinkUnchecked

instance GenUnchecked Double where
    genUnchecked = genDouble
#if MIN_VERSION_QuickCheck(2,9,2)
    shrinkUnchecked d = if
      | isInfinite d -> []
      | isNaN d -> []
      | otherwise -> shrink d
#else
    shrinkUnchecked _ = []
#endif

instance GenValid Double where
    genValid = genUnchecked
    shrinkValid = shrinkUnchecked

instance GenUnchecked Integer where
    genUnchecked = genInteger
    shrinkUnchecked = shrink

instance GenValid Integer

#if MIN_VERSION_base(4,8,0)
instance GenUnchecked Natural where
    genUnchecked = fromInteger . abs <$> genUnchecked
    shrinkUnchecked = fmap (fromInteger . abs) . shrinkUnchecked . toInteger

instance GenValid Natural where
    genValid = fromInteger . abs <$> genValid
#endif

instance (Integral a, GenUnchecked a) => GenUnchecked (Ratio a) where
    genUnchecked = (:%) <$> genUnchecked <*> genUnchecked
    shrinkUnchecked (n :% d) = [n' :% d' | (n', d') <- shrinkUnchecked (n, d)]

instance (Integral a, Num a, Ord a, GenValid a) => GenValid (Ratio a) where
    genValid = (do
      n <- genValid
      d <- (genValid `suchThat` (> 0))
      pure $ n :% d) `suchThat` ((== valid) . validateRatioNormalised)
    shrinkValid (n :% d) = filter isValid [n' % d' | (n', d') <- shrinkValid (n, d), d' > 0]

instance (Integral a, Num a, Ord a, Validity a, GenUnchecked a) => GenInvalid (Ratio a)

instance HasResolution a => GenUnchecked (Fixed a) where
    genUnchecked = MkFixed <$> genUnchecked
    shrinkUnchecked (MkFixed i) = MkFixed <$> shrinkUnchecked i

instance HasResolution a => GenValid (Fixed a)

genericGenUnchecked :: (Generic a, GGenUnchecked (Rep a)) => Gen a
genericGenUnchecked = to <$> gGenUnchecked

class GGenUnchecked f where
    gGenUnchecked :: Gen (f a)

instance GGenUnchecked U1 where
    gGenUnchecked = pure U1

instance (GGenUnchecked a, GGenUnchecked b) => GGenUnchecked (a :*: b) where
    gGenUnchecked = (:*:) <$> gGenUnchecked <*> gGenUnchecked

instance (GGenUnchecked a, GGenUnchecked b) => GGenUnchecked (a :+: b) where
    gGenUnchecked = oneof [L1 <$> gGenUnchecked, R1 <$> gGenUnchecked]

instance (GGenUnchecked a) => GGenUnchecked (M1 i c a) where
    gGenUnchecked = M1 <$> gGenUnchecked

instance (GenUnchecked a) => GGenUnchecked (K1 i a) where
    gGenUnchecked = K1 <$> genUnchecked


-- | Shrink a term to any of its immediate subterms,
-- and also recursively shrink all subterms.
genericShrinkUnchecked :: (Generic a, GUncheckedRecursivelyShrink (Rep a), GUncheckedSubterms (Rep a) a) => a -> [a]
genericShrinkUnchecked x = uncheckedSubterms x ++ uncheckedRecursivelyShrink x

-- | Recursively shrink all immediate uncheckedSubterms.
uncheckedRecursivelyShrink :: (Generic a, GUncheckedRecursivelyShrink (Rep a)) => a -> [a]
uncheckedRecursivelyShrink = map to . gUncheckedRecursivelyShrink . from

class GUncheckedRecursivelyShrink f where
  gUncheckedRecursivelyShrink :: f a -> [f a]

instance (GUncheckedRecursivelyShrink f, GUncheckedRecursivelyShrink g) => GUncheckedRecursivelyShrink (f :*: g) where
  gUncheckedRecursivelyShrink (x :*: y) =
    ((:*:) <$> gUncheckedRecursivelyShrink x <*> gUncheckedRecursivelyShrink y)
      ++ [ x' :*: y | x' <- gUncheckedRecursivelyShrink x ]
      ++ [ x :*: y' | y' <- gUncheckedRecursivelyShrink y ]

instance (GUncheckedRecursivelyShrink f, GUncheckedRecursivelyShrink g) => GUncheckedRecursivelyShrink (f :+: g) where
  gUncheckedRecursivelyShrink (L1 x) = map L1 (gUncheckedRecursivelyShrink x)
  gUncheckedRecursivelyShrink (R1 x) = map R1 (gUncheckedRecursivelyShrink x)

instance GUncheckedRecursivelyShrink f => GUncheckedRecursivelyShrink (M1 i c f) where
  gUncheckedRecursivelyShrink (M1 x) = map M1 (gUncheckedRecursivelyShrink x)

instance GenUnchecked a => GUncheckedRecursivelyShrink (K1 i a) where
  gUncheckedRecursivelyShrink (K1 x) = map K1 (shrinkUnchecked x)

instance GUncheckedRecursivelyShrink U1 where
  gUncheckedRecursivelyShrink U1 = []

instance GUncheckedRecursivelyShrink V1 where
  -- The empty type can't be shrunk to anything.
  gUncheckedRecursivelyShrink _ = []


-- | All immediate uncheckedSubterms of a term.
uncheckedSubterms :: (Generic a, GUncheckedSubterms (Rep a) a) => a -> [a]
uncheckedSubterms = gUncheckedSubterms . from


class GUncheckedSubterms f a where
  gUncheckedSubterms :: f a -> [a]

instance GUncheckedSubterms V1 a where
  gUncheckedSubterms _ = []

instance GUncheckedSubterms U1 a where
  gUncheckedSubterms U1 = []

instance (GUncheckedSubtermsIncl f a, GUncheckedSubtermsIncl g a) => GUncheckedSubterms (f :*: g) a where
  gUncheckedSubterms (l :*: r) = gUncheckedSubtermsIncl l ++ gUncheckedSubtermsIncl r

instance (GUncheckedSubtermsIncl f a, GUncheckedSubtermsIncl g a) => GUncheckedSubterms (f :+: g) a where
  gUncheckedSubterms (L1 x) = gUncheckedSubtermsIncl x
  gUncheckedSubterms (R1 x) = gUncheckedSubtermsIncl x

instance GUncheckedSubterms f a => GUncheckedSubterms (M1 i c f) a where
  gUncheckedSubterms (M1 x) = gUncheckedSubterms x

instance GUncheckedSubterms (K1 i a) b where
  gUncheckedSubterms (K1 _) = []


class GUncheckedSubtermsIncl f a where
  gUncheckedSubtermsIncl :: f a -> [a]

instance GUncheckedSubtermsIncl V1 a where
  gUncheckedSubtermsIncl _ = []

instance GUncheckedSubtermsIncl U1 a where
  gUncheckedSubtermsIncl U1 = []

instance (GUncheckedSubtermsIncl f a, GUncheckedSubtermsIncl g a) => GUncheckedSubtermsIncl (f :*: g) a where
  gUncheckedSubtermsIncl (l :*: r) = gUncheckedSubtermsIncl l ++ gUncheckedSubtermsIncl r

instance (GUncheckedSubtermsIncl f a, GUncheckedSubtermsIncl g a) => GUncheckedSubtermsIncl (f :+: g) a where
  gUncheckedSubtermsIncl (L1 x) = gUncheckedSubtermsIncl x
  gUncheckedSubtermsIncl (R1 x) = gUncheckedSubtermsIncl x

instance GUncheckedSubtermsIncl f a => GUncheckedSubtermsIncl (M1 i c f) a where
  gUncheckedSubtermsIncl (M1 x) = gUncheckedSubtermsIncl x

-- This is the important case: We've found a term of the same type.
instance OVERLAPPING_ GUncheckedSubtermsIncl (K1 i a) a where
  gUncheckedSubtermsIncl (K1 x) = [x]

instance OVERLAPPING_ GUncheckedSubtermsIncl (K1 i a) b where
  gUncheckedSubtermsIncl (K1 _) = []


-- | Generate a valid value by generating all the sub parts using the 'Generic' instance,
-- and trying that until a valid value has been generated
--
-- > genValidStructurally = genValidStructurallyWithoutExtraChecking `suchThat` isValid
--
-- This is probably the function that you are looking for.
-- If you do use this function to override `genValid`, you probably also want to use
-- 'shrinkValidStructurally' to override 'shrinkValid'.
genValidStructurally :: (Validity a, Generic a, GGenValid (Rep a)) => Gen a
genValidStructurally = genValidStructurallyWithoutExtraChecking `suchThat` isValid

-- | Generate a valid value by generating all the sub parts using the 'Generic' instance,
--
-- This generator is _not_ guaranteed to generate a valid value.
--
-- This is probably _not_ the function that you are looking for when overriding
-- `genValid` _unless_ the type in question has no _extra_ validity constraints on top of
-- the validity of its sub parts.
genValidStructurallyWithoutExtraChecking :: (Generic a, GGenValid (Rep a)) => Gen a
genValidStructurallyWithoutExtraChecking = to <$> gGenValid

class GGenValid f where
    gGenValid :: Gen (f a)

instance GGenValid U1 where
    gGenValid = pure U1

instance (GGenValid a, GGenValid b) => GGenValid (a :*: b) where
    gGenValid = (:*:) <$> gGenValid <*> gGenValid

instance (GGenValid a, GGenValid b) => GGenValid (a :+: b) where
    gGenValid = oneof [L1 <$> gGenValid, R1 <$> gGenValid]

instance (GGenValid a) => GGenValid (M1 i c a) where
    gGenValid = M1 <$> gGenValid

instance (GenValid a) => GGenValid (K1 i a) where
    gGenValid = K1 <$> genValid


-- | Shrink a term to any of its immediate valid subterms,
-- and also recursively shrink all subterms, and then filtering out the results that are not valid.
--
-- > shrinkValidStructurally = filter isValid . shrinkValidStructurallyWithoutExtraFiltering
--
-- This is probably the function that you are looking for.
shrinkValidStructurally :: (Validity a, Generic a, GValidRecursivelyShrink (Rep a), GValidSubterms (Rep a) a) => a -> [a]
shrinkValidStructurally = filter isValid . shrinkValidStructurallyWithoutExtraFiltering

-- | Shrink a term to any of its immediate valid subterms,
-- and also recursively shrink all subterms.
--
-- This shrinking function is _not_ guaranteed to shrink to valid values.
--
-- This is probably _not_ the function that you are looking for when overriding
-- `shrinkValid` _unless_ the type in question has no _extra_ validity constraints on top of
-- the validity of its sub parts.
shrinkValidStructurallyWithoutExtraFiltering :: (Generic a, GValidRecursivelyShrink (Rep a), GValidSubterms (Rep a) a) => a -> [a]
shrinkValidStructurallyWithoutExtraFiltering x = structurallyValidSubterms x ++ structurallyValidRecursivelyShrink x

-- | Recursively shrink all immediate structurally valid subterms.
structurallyValidRecursivelyShrink :: (Generic a, GValidRecursivelyShrink (Rep a)) => a -> [a]
structurallyValidRecursivelyShrink = map to . gValidRecursivelyShrink . from

class GValidRecursivelyShrink f where
  gValidRecursivelyShrink :: f a -> [f a]

instance (GValidRecursivelyShrink f, GValidRecursivelyShrink g) => GValidRecursivelyShrink (f :*: g) where
  gValidRecursivelyShrink (x :*: y) =
    ((:*:) <$> gValidRecursivelyShrink x <*> gValidRecursivelyShrink y)
      ++ [ x' :*: y | x' <- gValidRecursivelyShrink x ]
      ++ [ x :*: y' | y' <- gValidRecursivelyShrink y ]

instance (GValidRecursivelyShrink f, GValidRecursivelyShrink g) => GValidRecursivelyShrink (f :+: g) where
  gValidRecursivelyShrink (L1 x) = map L1 (gValidRecursivelyShrink x)
  gValidRecursivelyShrink (R1 x) = map R1 (gValidRecursivelyShrink x)

instance GValidRecursivelyShrink f => GValidRecursivelyShrink (M1 i c f) where
  gValidRecursivelyShrink (M1 x) = map M1 (gValidRecursivelyShrink x)

instance GenValid a => GValidRecursivelyShrink (K1 i a) where
  gValidRecursivelyShrink (K1 x) = map K1 (shrinkValid x)

instance GValidRecursivelyShrink U1 where
  gValidRecursivelyShrink U1 = []

instance GValidRecursivelyShrink V1 where
  -- The empty type can't be shrunk to anything.
  gValidRecursivelyShrink _ = []


-- | All immediate validSubterms of a term.
structurallyValidSubterms :: (Generic a, GValidSubterms (Rep a) a) => a -> [a]
structurallyValidSubterms = gValidSubterms . from


class GValidSubterms f a where
  gValidSubterms :: f a -> [a]

instance GValidSubterms V1 a where
  gValidSubterms _ = []

instance GValidSubterms U1 a where
  gValidSubterms U1 = []

instance (GValidSubtermsIncl f a, GValidSubtermsIncl g a) => GValidSubterms (f :*: g) a where
  gValidSubterms (l :*: r) = gValidSubtermsIncl l ++ gValidSubtermsIncl r

instance (GValidSubtermsIncl f a, GValidSubtermsIncl g a) => GValidSubterms (f :+: g) a where
  gValidSubterms (L1 x) = gValidSubtermsIncl x
  gValidSubterms (R1 x) = gValidSubtermsIncl x

instance GValidSubterms f a => GValidSubterms (M1 i c f) a where
  gValidSubterms (M1 x) = gValidSubterms x

instance GValidSubterms (K1 i a) b where
  gValidSubterms (K1 _) = []


class GValidSubtermsIncl f a where
  gValidSubtermsIncl :: f a -> [a]

instance GValidSubtermsIncl V1 a where
  gValidSubtermsIncl _ = []

instance GValidSubtermsIncl U1 a where
  gValidSubtermsIncl U1 = []

instance (GValidSubtermsIncl f a, GValidSubtermsIncl g a) => GValidSubtermsIncl (f :*: g) a where
  gValidSubtermsIncl (l :*: r) = gValidSubtermsIncl l ++ gValidSubtermsIncl r

instance (GValidSubtermsIncl f a, GValidSubtermsIncl g a) => GValidSubtermsIncl (f :+: g) a where
  gValidSubtermsIncl (L1 x) = gValidSubtermsIncl x
  gValidSubtermsIncl (R1 x) = gValidSubtermsIncl x

instance GValidSubtermsIncl f a => GValidSubtermsIncl (M1 i c f) a where
  gValidSubtermsIncl (M1 x) = gValidSubtermsIncl x

-- This is the important case: We've found a term of the same type.
instance OVERLAPPING_ GValidSubtermsIncl (K1 i a) a where
  gValidSubtermsIncl (K1 x) = [x]

instance OVERLAPPING_ GValidSubtermsIncl (K1 i a) b where
  gValidSubtermsIncl (K1 _) = []
