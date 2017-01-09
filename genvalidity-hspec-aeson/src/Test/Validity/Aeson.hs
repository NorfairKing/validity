{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Validity.Aeson
    ( jsonSpecOnValid
    , jsonSpec
    , jsonSpecOnArbitrary
    , jsonSpecOnGen
    , neverFailsToEncodeOnGen
    , encodeAndDecodeAreInversesOnGen
    ) where

import Data.GenValidity

import Control.DeepSeq (deepseq)
import Control.Exception (evaluate)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.Typeable
import Test.Hspec
import Test.QuickCheck
import Test.Validity.Utils

-- | Standard test spec for properties of JSON-related functions for valid values
--
-- Example usage:
--
-- > jsonSpecOnValid @Double
jsonSpecOnValid
    :: forall a.
       (Show a, Eq a, Typeable a, GenValid a, FromJSON a, ToJSON a)
    => Spec
jsonSpecOnValid = jsonSpecOnGen (genValid @a) "valid"

-- | Standard test spec for properties of JSON-related functions for unchecked values
--
-- Example usage:
--
-- > jsonSpec @Int
jsonSpec
    :: forall a.
       (Show a, Eq a, Typeable a, GenUnchecked a, FromJSON a, ToJSON a)
    => Spec
jsonSpec = jsonSpecOnGen (genUnchecked @a) "unchecked"

-- | Standard test spec for properties of JSON-related functions for arbitrary values
--
-- Example usage:
--
-- > jsonSpecOnArbitrary @Int
jsonSpecOnArbitrary
    :: forall a.
       (Show a, Eq a, Typeable a, Arbitrary a, FromJSON a, ToJSON a)
    => Spec
jsonSpecOnArbitrary = jsonSpecOnGen (arbitrary @a) "arbitrary"

-- | Standard test spec for properties of JSON-related functions for a given generator (and a name for that generator).
--
-- Example usage:
--
-- > jsonSpecOnGen (genListOf $ pure 'a') "sequence of 'a's"
jsonSpecOnGen
    :: forall a.
       (Show a, Eq a, Typeable a, FromJSON a, ToJSON a)
    => Gen a -> String -> Spec
jsonSpecOnGen gen genname = do
    let name = nameOf (Proxy @a)
    describe ("JSON " ++ name ++ " (" ++ genname ++ ")") $ do
        describe ("encode :: " ++ name ++ " -> Data.ByteString.Lazy.ByteString") $
            it
                (unwords
                     ["never fails to encode a", "\"" ++ genname, name ++ "\""]) $
            neverFailsToEncodeOnGen gen
        describe ("decode :: " ++ name ++ " -> Data.ByteString.Lazy.ByteString") $
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
    :: (Show a, ToJSON a)
    => Gen a -> Property
neverFailsToEncodeOnGen gen =
    forAll gen $ \(a :: a) ->
        evaluate (deepseq (JSON.encode a) ()) `shouldReturn` ()

-- |
--
-- prop> encodeAndDecodeAreInversesOnGen @Bool arbitrary
-- prop> encodeAndDecodeAreInversesOnGen @Bool genUnchecked
-- prop> encodeAndDecodeAreInversesOnGen @Bool genValid
-- prop> encodeAndDecodeAreInversesOnGen @Int arbitrary
-- prop> encodeAndDecodeAreInversesOnGen @Int genUnchecked
-- prop> encodeAndDecodeAreInversesOnGen @Int genValid
encodeAndDecodeAreInversesOnGen
    :: (Show a, Eq a, FromJSON a, ToJSON a)
    => Gen a -> Property
encodeAndDecodeAreInversesOnGen gen =
    forAll gen $ \(a :: a) ->
        JSON.eitherDecode (JSON.encode a) `shouldBe` Right a
