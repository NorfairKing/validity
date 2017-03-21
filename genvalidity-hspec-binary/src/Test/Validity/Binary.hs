{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Standard test `Spec`s and raw `Property`s for `Binary` instances.
--
-- You will need @TypeApplications@ to use these.
module Test.Validity.Binary
    ( binarySpecOnValid
    , binarySpec
    , binarySpecOnArbitrary
    , binarySpecOnGen
    , neverFailsToEncodeOnGen
    , encodeAndDecodeAreInversesOnGen
    ) where

import Data.GenValidity

import Control.DeepSeq (deepseq)
import Control.Exception (evaluate)
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import Data.Typeable
import Test.Hspec
import Test.QuickCheck
import Test.Validity.Utils

-- | Standard test spec for properties of 'Binary'-related functions for valid values
--
-- Example usage:
--
-- > BinarySpecOnValid @Double
binarySpecOnValid
    :: forall a.
       (Show a, Eq a, Typeable a, GenValid a, Binary a)
    => Spec
binarySpecOnValid = binarySpecOnGen (genValid @a) "valid"

-- | Standard test spec for properties of 'Binary'-related functions for unchecked values
--
-- Example usage:
--
-- > binarySpec @Int
binarySpec
    :: forall a.
       (Show a, Eq a, Typeable a, GenUnchecked a, Binary a)
    => Spec
binarySpec = binarySpecOnGen (genUnchecked @a) "unchecked"

-- | Standard test spec for properties of 'Binary'-related functions for arbitrary values
--
-- Example usage:
--
-- > binarySpecOnArbitrary @Int
binarySpecOnArbitrary
    :: forall a.
       (Show a, Eq a, Typeable a, Arbitrary a, Binary a)
    => Spec
binarySpecOnArbitrary = binarySpecOnGen (arbitrary @a) "arbitrary"

-- | Standard test spec for properties of 'Binary'-related functions for a given generator (and a name for that generator).
--
-- Example usage:
--
-- > binarySpecOnGen (genListOf $ pure 'a') "sequence of 'a's"
binarySpecOnGen
    :: forall a.
       (Show a, Eq a, Typeable a, Binary a)
    => Gen a -> String -> Spec
binarySpecOnGen gen genname =
    parallel $ do
        let name = nameOf @a
        describe ("Binary " ++ name ++ " (" ++ genname ++ ")") $ do
            describe ("encode :: " ++ name ++ " -> Data.ByteString.ByteString") $
                it
                    (unwords
                         [ "never fails to encode a"
                         , "\"" ++ genname
                         , name ++ "\""
                         ]) $
                neverFailsToEncodeOnGen gen
            describe ("decode :: " ++ name ++ " -> Data.ByteString.ByteString") $
                it
                    (unwords
                         [ "ensures that encode and decode are inverses for"
                         , "\"" ++ genname
                         , name ++ "\"" ++ "'s"
                         ]) $
                encodeAndDecodeAreInversesOnGen gen

-- |
--
-- prop> neverFailsToEncodeOnGen @Bool arbitrary
-- prop> neverFailsToEncodeOnGen @Bool genUnchecked
-- prop> neverFailsToEncodeOnGen @Bool genValid
-- prop> neverFailsToEncodeOnGen @Int arbitrary
-- prop> neverFailsToEncodeOnGen @Int genUnchecked
-- prop> neverFailsToEncodeOnGen @Int genValid
neverFailsToEncodeOnGen
    :: (Show a, Binary a)
    => Gen a -> Property
neverFailsToEncodeOnGen gen =
    forAll gen $ \(a :: a) ->
        evaluate (deepseq (Binary.encode a) ()) `shouldReturn` ()

-- |
--
-- prop> encodeAndDecodeAreInversesOnGen @Bool arbitrary
-- prop> encodeAndDecodeAreInversesOnGen @Bool genUnchecked
-- prop> encodeAndDecodeAreInversesOnGen @Bool genValid
-- prop> encodeAndDecodeAreInversesOnGen @Int arbitrary
-- prop> encodeAndDecodeAreInversesOnGen @Int genUnchecked
-- prop> encodeAndDecodeAreInversesOnGen @Int genValid
encodeAndDecodeAreInversesOnGen
    :: (Show a, Eq a, Binary a)
    => Gen a -> Property
encodeAndDecodeAreInversesOnGen gen =
    forAll gen $ \(a :: a) ->
        Binary.decode (Binary.encode a) `shouldBe` Right a
