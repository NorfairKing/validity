{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for GenRelativeValidity instances
--
-- You will need @TypeApplications@ to use these.
module Test.Validity.GenRelativeValidity
  ( genRelativeValiditySpec,
    genRelativeValidSpec,
    genRelativeInvalidSpec,
    genRelativeValidGeneratesValid,
    genRelativeInvalidGeneratesInvalid,
  )
where

import Data.Data
import Data.GenRelativeValidity
import Data.GenValidity
import Test.Hspec
import Test.QuickCheck
import Test.Validity.Property.Utils
import Test.Validity.Utils

-- | A @Spec@ that specifies that @genValidFor@ and @genInvalidFor@ work as
-- intended.
--
-- In general it is a good idea to add this spec to your test suite if you
-- write a custom implementation of @genValidFor@ or @genInvalidFor@.
--
-- Example usage:
--
-- > relativeGenValiditySpec @MyDataFor @MyOtherData
genRelativeValiditySpec ::
  forall a b.
  ( Typeable a,
    Show a,
    Show b,
    GenUnchecked b,
    GenValid b,
    GenRelativeValid a b,
    GenRelativeInvalid a b
  ) =>
  Spec
genRelativeValiditySpec = do
  genRelativeValidSpec @a @b
  genRelativeInvalidSpec @a @b

genRelativeValidSpec ::
  forall a b.
  ( Typeable a,
    Show a,
    Show b,
    GenValid b,
    GenRelativeValid a b
  ) =>
  Spec
genRelativeValidSpec =
  parallel $ do
    let nameOne = nameOf @a
    let nameTwo = nameOf @a
    describe ("GenRelativeValidity " ++ nameOne ++ " " ++ nameTwo) $
      describe ("genValidFor   :: " ++ nameTwo ++ " -> Gen " ++ nameOne) $
        it
          ( "only generates valid \'"
              ++ nameOne
              ++ "\'s for the "
              ++ nameTwo
          )
          $ genRelativeValidGeneratesValid @a @b

genRelativeInvalidSpec ::
  forall a b.
  ( Typeable a,
    Show a,
    Show b,
    GenUnchecked b,
    GenRelativeInvalid a b
  ) =>
  Spec
genRelativeInvalidSpec =
  parallel $ do
    let nameOne = nameOf @a
    let nameTwo = nameOf @a
    describe ("GenRelativeInvalid " ++ nameOne ++ " " ++ nameTwo) $
      describe ("genInvalidFor   :: " ++ nameTwo ++ " -> Gen " ++ nameOne) $
        it
          ( "only generates invalid \'"
              ++ nameOne
              ++ "\'s for the "
              ++ nameTwo
          )
          $ genRelativeInvalidGeneratesInvalid @a @b

-- | @genValidFor b@ only generates values that satisfy @isValidFor b@
genRelativeValidGeneratesValid ::
  forall a b.
  (Show a, Show b, GenValid b, GenRelativeValid a b) =>
  Property
genRelativeValidGeneratesValid =
  forAllValid $ \(b :: b) ->
    forAll (genValidFor b) $ \(a :: a) -> a `shouldSatisfy` (`isValidFor` b)

-- | @genInvalidFor b@ only generates values that do not satisfy @isValidFor b@
genRelativeInvalidGeneratesInvalid ::
  forall a b.
  ( Show a,
    Show b,
    GenUnchecked b,
    GenRelativeInvalid a b
  ) =>
  Property
genRelativeInvalidGeneratesInvalid =
  forAllUnchecked $ \(b :: b) ->
    forAll (genInvalidFor b) $ \(a :: a) ->
      a `shouldNotSatisfy` (`isValidFor` b)
