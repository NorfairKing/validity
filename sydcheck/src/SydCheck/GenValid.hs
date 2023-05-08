{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module SydCheck.GenValid where

import Data.Int
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Ratio
import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Vector ()
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
import Data.Word
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import GHC.Generics
import Numeric.Natural
import SydCheck.Gen

-- Laws:
-- 1: Every generated value must be valid
-- 2: With enough randomness, every valid value must be generated eventually.
-- 3: Annoying values should have increased likelihood of being generated.
--
-- Ideally the generated values are particularly anoying.
-- So we try to generate values around the bounds with increased probability
class Validity a => GenValid a where
  genValid :: Gen a
  default genValid :: (Generic a, GGenValid (Rep a)) => Gen a
  genValid = genValidStructurally

instance GenValid () where
  genValid = pure ()

instance GenValid Bool where
  genValid = genBool False

instance GenValid Ordering where
  genValid = genOrdering LT

instance GenValid Char where
  genValid = genChar (minBound, maxBound)

instance GenValid Int8 where
  genValid = genIntX

instance GenValid Int16 where
  genValid = genIntX

instance GenValid Int32 where
  genValid = genIntX

instance GenValid Int64 where
  genValid = genIntX

instance GenValid Int where
  genValid = genIntX

instance GenValid Word8 where
  genValid = genWordX

instance GenValid Word16 where
  genValid = genWordX

instance GenValid Word32 where
  genValid = genWordX

instance GenValid Word64 where
  genValid = genWordX

instance GenValid Word where
  genValid = genWordX

instance GenValid Float where
  genValid = genFloatX (castWord32ToFloat . fromIntegral)

instance GenValid Double where
  genValid = genFloatX castWord64ToDouble

instance GenValid Integer where
  genValid = genInteger

instance GenValid Natural where
  genValid = fromInteger . abs <$> genValid

instance (Integral a, GenValid a) => GenValid (Ratio a) where
  genValid = genRatioOf genValid

instance (GenValid a, GenValid b) => GenValid (a, b) where
  genValid = (,) <$> genValid <*> genValid

instance (GenValid a, GenValid b, GenValid c) => GenValid (a, b, c) where
  genValid = (,,) <$> genValid <*> genValid <*> genValid

instance (GenValid a, GenValid b, GenValid c, GenValid d) => GenValid (a, b, c, d) where
  genValid = (,,,) <$> genValid <*> genValid <*> genValid <*> genValid

instance (GenValid a, GenValid b, GenValid c, GenValid d, GenValid e) => GenValid (a, b, c, d, e) where
  genValid = (,,,,) <$> genValid <*> genValid <*> genValid <*> genValid <*> genValid

instance (GenValid a, GenValid b, GenValid c, GenValid d, GenValid e, GenValid f) => GenValid (a, b, c, d, e, f) where
  genValid = (,,,,,) <$> genValid <*> genValid <*> genValid <*> genValid <*> genValid <*> genValid

instance GenValid a => GenValid (Maybe a) where
  genValid = genMaybeOf genValid

instance (GenValid a, GenValid b) => GenValid (Either a b) where
  genValid = genEitherOf genValid genValid

instance GenValid a => GenValid [a] where
  genValid = genListOf genValid

instance GenValid a => GenValid (NonEmpty a) where
  genValid = genNonEmptyOf genValid

instance (Show k, Ord k, GenValid k, GenValid v) => GenValid (Map k v) where
  genValid = genMapOf genValid

instance GenValid a => GenValid (V.Vector a) where
  genValid = genVectorOf genValid

instance (GenValid a, SV.Storable a) => GenValid (SV.Vector a) where
  genValid = genStorableVectorOf genValid

instance (GenValid a, UV.Unbox a) => GenValid (UV.Vector a) where
  genValid = genUnboxedVectorOf genValid

-- | Generate a valid value by generating all the sub parts using the 'Generic' instance,
-- and trying that until a valid value has been generated
--
-- > genValidStructurally = genValidStructurallyWithoutExtraChecking `suchThat` isValid
--
-- This is probably the function that you are looking for.
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
