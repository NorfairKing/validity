{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Standard test `Spec`s and raw `Property`s for `FromJSON` and `ToJSON` instances.
--
-- You will need @TypeApplications@ to use these.
module Test.Validity.Aeson
  ( jsonSpec,
    jsonSpecOnArbitrary,
    jsonSpecOnGen,
    neverFailsToEncodeOnGen,
    encodeAndDecodeAreInversesOnGen,
  )
where

import Control.DeepSeq (deepseq)
import Control.Exception (evaluate)
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.GenValidity
import Data.Typeable
import Test.Hspec
import Test.QuickCheck
import Test.Validity.Utils

-- | Standard test spec for properties of JSON-related functions for valid values
--
-- Example usage:
--
-- > jsonSpec @Int
jsonSpec ::
  forall a.
  (Show a, Eq a, Typeable a, GenValid a, FromJSON a, ToJSON a) =>
  Spec
jsonSpec = jsonSpecOnGen (genValid @a) "valid" shrinkValid

-- | Standard test spec for properties of JSON-related functions for arbitrary values
--
-- Example usage:
--
-- > jsonSpecOnArbitrary @Int
jsonSpecOnArbitrary ::
  forall a.
  (Show a, Eq a, Typeable a, Arbitrary a, FromJSON a, ToJSON a) =>
  Spec
jsonSpecOnArbitrary = jsonSpecOnGen (arbitrary @a) "arbitrary" shrink

-- | Standard test spec for properties of JSON-related functions for a given generator (and a name for that generator).
--
-- Example usage:
--
-- > jsonSpecOnGen (genListOf $ pure 'a') "sequence of 'a's"
jsonSpecOnGen ::
  forall a.
  (Show a, Eq a, Typeable a, FromJSON a, ToJSON a) =>
  Gen a ->
  String ->
  (a -> [a]) ->
  Spec
jsonSpecOnGen gen genname s =
  parallel $ do
    let name = nameOf @a
    describe ("JSON " ++ name ++ " (" ++ genname ++ ")") $ do
      describe
        ("encode :: " ++ name ++ " -> Data.ByteString.Lazy.ByteString")
        $ it
          ( unwords
              [ "never fails to encode a",
                "\"" ++ genname,
                name ++ "\""
              ]
          )
        $ neverFailsToEncodeOnGen gen s
      describe
        ( "decode :: Data.ByteString.Lazy.ByteString -> Either String "
            ++ name
        )
        $ it
          ( unwords
              [ "ensures that encode and decode are inverses for",
                "\"" ++ genname,
                name ++ "\"" ++ "'s"
              ]
          )
        $ encodeAndDecodeAreInversesOnGen gen s

-- |
--
-- prop> neverFailsToEncodeOnGen @Bool arbitrary shrink
-- prop> neverFailsToEncodeOnGen @Bool genValid shrinkValid
-- prop> neverFailsToEncodeOnGen @Bool genValid shrinkValid
-- prop> neverFailsToEncodeOnGen @Int arbitrary shrink
-- prop> neverFailsToEncodeOnGen @Int genValid shrinkValid
-- prop> neverFailsToEncodeOnGen @Int genValid shrinkValid
neverFailsToEncodeOnGen :: (Show a, ToJSON a) => Gen a -> (a -> [a]) -> Property
neverFailsToEncodeOnGen gen s =
  forAllShrink gen s $ \(a :: a) ->
    evaluate (deepseq (JSON.encode a) ()) `shouldReturn` ()

-- |
--
-- prop> encodeAndDecodeAreInversesOnGen @Bool arbitrary shrink
-- prop> encodeAndDecodeAreInversesOnGen @Bool genValid shrinkValid
-- prop> encodeAndDecodeAreInversesOnGen @Bool genValid shrinkValid
-- prop> encodeAndDecodeAreInversesOnGen @Int arbitrary shrink
-- prop> encodeAndDecodeAreInversesOnGen @Int genValid shrinkValid
-- prop> encodeAndDecodeAreInversesOnGen @Int genValid shrinkValid
encodeAndDecodeAreInversesOnGen ::
  (Show a, Eq a, FromJSON a, ToJSON a) => Gen a -> (a -> [a]) -> Property
encodeAndDecodeAreInversesOnGen gen s =
  forAllShrink gen s $ \(a :: a) ->
    let encoded = JSON.encode a
        errOrDecoded = JSON.eitherDecode encoded
     in case errOrDecoded of
          Left err ->
            expectationFailure $
              unlines
                [ "Decoding failed with error",
                  err,
                  "instead of decoding to",
                  show a,
                  "'encode' encoded it to the json",
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
                    "'encode' encoded it to the json",
                    show encoded
                  ]
