{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for GenValidity instances
--
-- You will need @TypeApplications@ to use these.
module Test.Validity.GenValidity
  ( genValidSpec,
    genValidGeneratesValid,
    genGeneratesValid,
    genGeneratesInvalid,
  )
where

import Data.Data
import Data.GenValidity
import Test.Hspec
import Test.QuickCheck
import Test.Validity.GenValidity.Property
import Test.Validity.Utils

-- | A @Spec@ that specifies that @genValid@ only generates valid data.
--
-- In general it is a good idea to add this spec to your test suite if you
-- write a custom implementation of @genValid@.
--
-- Example usage:
--
-- > genValidSpec @Int
genValidSpec ::
  forall a.
  (Typeable a, Show a, GenValid a) =>
  Spec
genValidSpec =
  parallel $ do
    let name = nameOf @a
    describe ("GenValid " ++ name) $
      describe ("genValid   :: Gen " ++ name) $
        it ("only generates valid \'" ++ name ++ "\'s") $
          genValidGeneratesValid @a

-- | @genValid@ only generates valid data
--
-- prop> genValidGeneratesValid @()
-- prop> genValidGeneratesValid @Bool
-- prop> genValidGeneratesValid @Ordering
-- prop> genValidGeneratesValid @Char
-- prop> genValidGeneratesValid @Int
-- prop> genValidGeneratesValid @Float
-- prop> genValidGeneratesValid @Double
-- prop> genValidGeneratesValid @Integer
-- prop> genValidGeneratesValid @(Maybe Int)
-- prop> genValidGeneratesValid @[Int]
genValidGeneratesValid ::
  forall a.
  (Show a, GenValid a) =>
  Property
genValidGeneratesValid = genGeneratesValid @a genValid
