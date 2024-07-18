{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- |
--
--    @GenValid@ exists to make tests involving @Validity@ types easier and
--    speed up the generation of data for them.
--
--    To implement tests for this datatype, we would have to be able to
--    generate both primes. We could do this with a generator like this one:
--
--    > (Prime <$> 'arbitrary') `suchThat` isValid
--
--    However, this is tedious and inefficient, as well as quite
--    naive (because 'arbitrary' tends to use very naive generators).
--
--    The @GenValid@ type class allows you to specify how to (efficiently)
--    generate valid data of the given type to allow for easier and quicker testing.
--    The default implementation of `GenValid` already gives you a generator and shrinking function
--    for free:
--
--    > instance GenValid Prime
--
--    For example, to generate primes, we don't have to consider even numbers other
--    than 2. A more efficient implementation could then look as follows:
--
--    > instance GenValid Prime where
--    >     genValid = Prime <$>
--    >        (oneof
--    >          [ pure 2
--    >          , ((\y -> 2 * abs y + 1) <$> arbitrary) `suchThat` isPrime)
--    >          ])
--
--
--    Typical examples of tests involving validity could look as follows:
--
--    > it "succeeds when given valid input" $ do
--    >     forAllValid $ \input ->
--    >         myFunction input `shouldSatisfy` isRight
--
--    > it "produces valid output when it succeeds" $ do
--    >     forAllValid $ \input ->
--    >         case myFunction input of
--    >             Nothing -> return () -- Can happen
--    >             Just output -> output `shouldSatisfy` isValid
--
--    Definitely also look at the companion packages for more info on how to use this package.
module Data.GenValidity
  ( GenValid (..),

    -- * Helper functions
    genValidStructurally,
    genValidStructurallyWithoutExtraChecking,
    shrinkValidStructurally,
    shrinkValidStructurallyWithoutExtraFiltering,
    module Data.GenValidity.Utils,

    -- ** Helper functions for specific types

    -- *** Char
    genUtf16SurrogateCodePoint,
    genLineSeparator,
    genNonLineSeparator,

    -- *** String
    genSingleLineString,

    -- * Re-exports
    module Data.Validity,

    -- * The Generics magic
    GGenValid (..),
    GValidRecursivelyShrink (..),
    structurallyValidSubterms,
    GValidSubterms (..),
    GValidSubtermsIncl (..),
  )
where

import Control.Monad (guard)
import Data.Char (chr)
import Data.Fixed (Fixed (..), HasResolution)
import Data.Functor.Const (Const (Const))
import Data.Functor.Identity (Identity (Identity))
import Data.GenValidity.Utils
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid (Alt, Dual)
import qualified Data.Monoid as Monoid
import Data.Ratio ((%))
import qualified Data.Semigroup as Semigroup
import Data.Validity
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
import GHC.Real (Ratio (..))
import Numeric.Natural
import Test.QuickCheck hiding (Fixed)

{-# ANN module "HLint: ignore Reduce duplication" #-}

-- | A class of types for which valid values can be generated to be valid.
--
-- === How to instantiate 'GenValid'
--
-- __Step 1__: Try to instantiate 'GenValid' without overriding any functions.
--             It is possible that, if few values are valid or if validity
--             checking is expensive, the resulting generator is too slow.
--             In that case, go to Step 2.
--
-- __Step 2__: Consider using 'genValidStructurallyWithoutExtraChecking' and
--             'shrinkValidStructurallyWithoutExtraFiltering' to speed up generation.
--             This only works if your type has a derived or trivial 'Validity'
--             instance.
--
-- __Step 3__: If that still is not fast enough, consider writing your own
--             generator and shrinking function.
--             Make sure to generate any possible valid value, but only valid values.
--
-- === A note about 'Arbitrary'
--
-- If you also write @Arbitrary@ instances for @GenValid@ types, it may be
-- best to simply use
--
-- > instance Arbitrary A where
-- >   arbitrary = genValid
-- >   shrink = shrinkValid
class (Validity a) => GenValid a where
  -- | Generate a valid datum, this should cover all possible valid values in
  -- the type
  --
  -- The default implementation is as follows:
  --
  -- >  genValid = genValidStructurally
  --
  -- To speed up testing, it may be a good idea to implement this yourself.
  -- If you do, make sure that it is possible to generate all possible valid
  -- data, otherwise your testing may not cover all cases.
  genValid :: Gen a
  default genValid :: (Generic a, GGenValid (Rep a)) => Gen a
  genValid = genValidStructurally

  -- | Shrink a valid value.
  --
  -- The default implementation is as follows:
  --
  -- >  shrinkValid = shrinkValidStructurally
  --
  -- It is important that this shrinking function only shrinks values to valid values.
  -- If `shrinkValid` ever shrinks a value to an invalid value, the test that is being shrunk for
  -- might fail for a different reason than for the reason that it originally failed.
  -- This would lead to very confusing error messages.
  shrinkValid :: a -> [a]
  default shrinkValid :: (Generic a, GValidRecursivelyShrink (Rep a), GValidSubterms (Rep a) a) => a -> [a]
  shrinkValid = shrinkValidStructurally

instance (GenValid a, GenValid b) => GenValid (a, b) where
  genValid =
    sized $ \n -> do
      (r, s) <- genSplit n
      a <- resize r genValid
      b <- resize s genValid
      return (a, b)
  shrinkValid = shrinkTuple shrinkValid shrinkValid

instance (GenValid a, GenValid b) => GenValid (Either a b) where
  genValid = oneof [Left <$> genValid, Right <$> genValid]
  shrinkValid (Left a) = Left <$> shrinkValid a
  shrinkValid (Right b) = Right <$> shrinkValid b

instance (GenValid a, GenValid b, GenValid c) => GenValid (a, b, c) where
  genValid =
    sized $ \n -> do
      (r, s, t) <- genSplit3 n
      a <- resize r genValid
      b <- resize s genValid
      c <- resize t genValid
      return (a, b, c)
  shrinkValid = shrinkTriple shrinkValid shrinkValid shrinkValid

instance
  (GenValid a, GenValid b, GenValid c, GenValid d) =>
  GenValid (a, b, c, d)
  where
  genValid =
    sized $ \n -> do
      (r, s, t, u) <- genSplit4 n
      a <- resize r genValid
      b <- resize s genValid
      c <- resize t genValid
      d <- resize u genValid
      return (a, b, c, d)
  shrinkValid = shrinkQuadruple shrinkValid shrinkValid shrinkValid shrinkValid

instance
  (GenValid a, GenValid b, GenValid c, GenValid d, GenValid e) =>
  GenValid (a, b, c, d, e)
  where
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

instance (GenValid a) => GenValid (Maybe a) where
  genValid = genMaybe genValid
  shrinkValid = shrinkMaybe shrinkValid

instance (GenValid a) => GenValid (NonEmpty a) where
  genValid = genNonEmptyOf genValid
  shrinkValid = shrinkNonEmpty shrinkValid

instance (GenValid a) => GenValid [a] where
  genValid = genListOf genValid
  shrinkValid = shrinkList shrinkValid

instance GenValid () where
  genValid = pure ()
  shrinkValid () = []

instance GenValid Bool where
  genValid = arbitrary
  shrinkValid = shrink

instance GenValid Ordering where
  genValid = arbitrary
  shrinkValid = shrink

instance GenValid Char where
  genValid =
    frequency
      [ (9, choose (minBound, maxBound)),
        (1, genUtf16SurrogateCodePoint)
      ]
  shrinkValid = shrink

genUtf16SurrogateCodePoint :: Gen Char
genUtf16SurrogateCodePoint = chr <$> oneof [choose (0xD800, 0xDBFF), choose (0xDC00, 0xDFFF)]

genLineSeparator :: Gen Char
genLineSeparator = elements ['\n', '\r']

genNonLineSeparator :: Gen Char
genNonLineSeparator = genValid `suchThat` (not . isLineSeparator)

genSingleLineString :: Gen String
genSingleLineString = genListOf genNonLineSeparator

instance GenValid Int where
  genValid = genIntX
  shrinkValid = shrink

instance GenValid Int8 where
  genValid = genIntX
  shrinkValid = shrink

instance GenValid Int16 where
  genValid = genIntX
  shrinkValid = shrink

instance GenValid Int32 where
  genValid = genIntX
  shrinkValid = shrink

instance GenValid Int64 where
  genValid = genIntX
  shrinkValid = shrink

instance GenValid Word where
  genValid = genWordX
  shrinkValid = shrink

instance GenValid Word8 where
  genValid = genWordX
  shrinkValid = shrink

instance GenValid Word16 where
  genValid = genWordX
  shrinkValid = shrink

instance GenValid Word32 where
  genValid = genWordX
  shrinkValid = shrink

instance GenValid Word64 where
  genValid = genWordX
  shrinkValid = shrink

instance GenValid Float where
  genValid = genFloat
  shrinkValid f
    | isInfinite f = []
    | isNaN f = []
    | otherwise = shrink f

instance GenValid Double where
  genValid = genDouble
  shrinkValid d
    | isInfinite d = []
    | isNaN d = []
    | otherwise = shrink d

instance GenValid Integer where
  genValid = genInteger
  shrinkValid = shrink

instance GenValid Natural where
  genValid = fromInteger . abs <$> genValid
  shrinkValid = fmap (fromInteger . abs) . shrinkValid . toInteger

instance (Integral a, Num a, Ord a, GenValid a) => GenValid (Ratio a) where
  genValid =
    ( do
        n <- genValid
        d <- (genValid `suchThat` (> 0))
        pure $ n :% d
    )
      `suchThat` isValid
  shrinkValid (n :% d) = do
    (n', d') <- shrinkTuple shrinkValid (filter (> 0) . shrinkValid) (n, d)
    let candidate = n' :% d'
    guard $ isValid candidate
    pure $ n' % d'

instance (HasResolution a) => GenValid (Fixed a) where
  genValid = MkFixed <$> genValid
  shrinkValid (MkFixed i) = MkFixed <$> shrinkValid i

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

deriving newtype instance (GenValid a) => GenValid (Identity a)

deriving newtype instance (GenValid (f a)) => GenValid (Alt f a)

deriving newtype instance (GenValid a) => GenValid (Dual a)

deriving newtype instance (GenValid a) => GenValid (Semigroup.First a)

deriving newtype instance (GenValid a) => GenValid (Semigroup.Last a)

deriving newtype instance (GenValid a) => GenValid (Monoid.First a)

deriving newtype instance (GenValid a) => GenValid (Monoid.Last a)

deriving newtype instance (GenValid a) => GenValid (Const a b)

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
      ++ [x' :*: y | x' <- gValidRecursivelyShrink x]
      ++ [x :*: y' | y' <- gValidRecursivelyShrink y]

instance (GValidRecursivelyShrink f, GValidRecursivelyShrink g) => GValidRecursivelyShrink (f :+: g) where
  gValidRecursivelyShrink (L1 x) = map L1 (gValidRecursivelyShrink x)
  gValidRecursivelyShrink (R1 x) = map R1 (gValidRecursivelyShrink x)

instance (GValidRecursivelyShrink f) => GValidRecursivelyShrink (M1 i c f) where
  gValidRecursivelyShrink (M1 x) = map M1 (gValidRecursivelyShrink x)

instance (GenValid a) => GValidRecursivelyShrink (K1 i a) where
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

instance (GValidSubterms f a) => GValidSubterms (M1 i c f) a where
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

instance (GValidSubtermsIncl f a) => GValidSubtermsIncl (M1 i c f) a where
  gValidSubtermsIncl (M1 x) = gValidSubtermsIncl x

-- This is the important case: We've found a term of the same type.
instance {-# OVERLAPPING #-} GValidSubtermsIncl (K1 i a) a where
  gValidSubtermsIncl (K1 x) = [x]

instance {-# OVERLAPPING #-} GValidSubtermsIncl (K1 i a) b where
  gValidSubtermsIncl (K1 _) = []
