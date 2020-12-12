{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Standard test `Spec`s and raw `Property`s for `PersistField` instances.
--
-- You will need @TypeApplications@ to use these.
module Test.Syd.Validity.Persist
  ( persistSpecOnValid,
    persistSpec,
    persistSpecOnArbitrary,
    persistSpecOnGen,
    fromPersistValueAndToPersistValueAreInversesOnGen,
  )
where

import Control.Monad
import Data.GenValidity
import qualified Data.Text as T
import Data.Typeable
import Database.Persist (PersistField (..))
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity.Utils

-- | Standard test spec for properties of persistent-related functions for valid values
--
-- Example usage:
--
-- > persistSpecOnValid @Rational
persistSpecOnValid ::
  forall a.
  (Show a, Eq a, Typeable a, GenValid a, PersistField a) =>
  Spec
persistSpecOnValid = persistSpecOnGen (genValid @a) "valid" shrinkValid

-- | Standard test spec for properties of persistent-related functions for unchecked values
--
-- Example usage:
--
-- > persistSpec @Int
persistSpec ::
  forall a.
  (Show a, Eq a, Typeable a, GenUnchecked a, PersistField a) =>
  Spec
persistSpec = persistSpecOnGen (genUnchecked @a) "unchecked" shrinkUnchecked

-- | Standard test spec for properties of persistent-related functions for arbitrary values
--
-- Example usage:
--
-- > persistSpecOnArbitrary @Int
persistSpecOnArbitrary ::
  forall a.
  (Show a, Eq a, Typeable a, Arbitrary a, PersistField a) =>
  Spec
persistSpecOnArbitrary = persistSpecOnGen (arbitrary @a) "arbitrary" shrink

-- | Standard test spec for properties of persistent-related functions for a given generator (and a name for that generator).
--
-- Example usage:
--
-- > persistSpecOnGen (genListOf $ pure 'a') "sequence of 'a's"
persistSpecOnGen ::
  forall a.
  (Show a, Eq a, Typeable a, PersistField a) =>
  Gen a ->
  String ->
  (a -> [a]) ->
  Spec
persistSpecOnGen gen genname s =
  parallel $ do
    let name = nameOf @a
    describe ("PersistField " ++ name ++ " (" ++ genname ++ ")") $ do
      describe ("fromPersistValue :: PersistValue -> Either Text " ++ name) $
        it
          ( unwords
              [ "ensures that toPersistValue and fromPersistValue are inverses for",
                "\"" ++ genname,
                name ++ "\"" ++ "'s"
              ]
          )
          $ fromPersistValueAndToPersistValueAreInversesOnGen gen s

-- |
--
-- prop> fromPersistValueAndToPersistValueAreInversesOnGen @Bool arbitrary shrink
--
-- prop> fromPersistValueAndToPersistValueAreInversesOnGen @Bool genUnchecked shrinkUnchecked
--
-- prop> fromPersistValueAndToPersistValueAreInversesOnGen @Bool genValid shrinkValid
--
-- prop> fromPersistValueAndToPersistValueAreInversesOnGen @Int arbitrary shrink
--
-- prop> fromPersistValueAndToPersistValueAreInversesOnGen @Int genUnchecked shrinkUnchecked
--
-- prop> fromPersistValueAndToPersistValueAreInversesOnGen @Int genValid shrinkValid
fromPersistValueAndToPersistValueAreInversesOnGen ::
  (Show a, Eq a, PersistField a) => Gen a -> (a -> [a]) -> Property
fromPersistValueAndToPersistValueAreInversesOnGen gen s =
  forAllShrink gen s $ \(a :: a) ->
    let encoded = toPersistValue a
        errOrDecoded = fromPersistValue encoded
     in case errOrDecoded of
          Left err ->
            expectationFailure $
              unlines
                [ "Decoding failed with error",
                  T.unpack err,
                  "instead of decoding to",
                  show a,
                  "'encode' encoded it to the persist",
                  show encoded
                ]
          Right decoded ->
            unless (decoded == a) $
              expectationFailure $
                unlines
                  [ "Decoding succeeded, but the decoded value",
                    show decoded,
                    "differs from expected decoded value",
                    show a,
                    "'encode' encoded it to the persist",
                    show encoded
                  ]
