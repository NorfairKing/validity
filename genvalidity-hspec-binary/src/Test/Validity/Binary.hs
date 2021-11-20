{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Standard test `Spec`s and raw `Property`s for `Binary` instances.
--
-- You will need @TypeApplications@ to use these.
module Test.Validity.Binary
  ( binarySpec,
    binarySpecOnArbitrary,
    binarySpecOnGen,
    neverFailsToEncodeOnGen,
    encodeAndDecodeAreInversesOnGen,
  )
where

import Control.DeepSeq (deepseq)
import Control.Exception (evaluate)
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Data.GenValidity
import Data.Typeable
import Test.Hspec
import Test.QuickCheck
import Test.Validity.Utils

-- | Standard test spec for properties of 'Binary'-related functions for valid values
--
-- Example usage:
--
-- > binarySpec @Int
binarySpec ::
  forall a.
  (Show a, Eq a, Typeable a, GenValid a, Binary a) =>
  Spec
binarySpec = binarySpecOnGen (genValid @a) "valid" shrinkValid

-- | Standard test spec for properties of 'Binary'-related functions for arbitrary values
--
-- Example usage:
--
-- > binarySpecOnArbitrary @Int
binarySpecOnArbitrary ::
  forall a.
  (Show a, Eq a, Typeable a, Arbitrary a, Binary a) =>
  Spec
binarySpecOnArbitrary = binarySpecOnGen (arbitrary @a) "arbitrary" shrink

-- | Standard test spec for properties of 'Binary'-related functions for a given generator (and a name for that generator).
--
-- Example usage:
--
-- > binarySpecOnGen (genListOf $ pure 'a') "sequence of 'a's" (const [])
binarySpecOnGen ::
  forall a.
  (Show a, Eq a, Typeable a, Binary a) =>
  Gen a ->
  String ->
  (a -> [a]) ->
  Spec
binarySpecOnGen gen genname s =
  parallel $ do
    let name = nameOf @a
    describe ("Binary " ++ name ++ " (" ++ genname ++ ")") $ do
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
        ("decode :: " ++ name ++ " -> Data.ByteString.Lazy.ByteString")
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
neverFailsToEncodeOnGen :: (Show a, Binary a) => Gen a -> (a -> [a]) -> Property
neverFailsToEncodeOnGen gen s =
  forAllShrink gen s $ \(a :: a) ->
    evaluate (deepseq (Binary.encode a) ()) `shouldReturn` ()

-- |
--
-- prop> encodeAndDecodeAreInversesOnGen @Bool arbitrary shrinkValid
-- prop> encodeAndDecodeAreInversesOnGen @Bool genValid shrinkValid
-- prop> encodeAndDecodeAreInversesOnGen @Bool genValid shrinkValid
-- prop> encodeAndDecodeAreInversesOnGen @Int arbitrary shrink
-- prop> encodeAndDecodeAreInversesOnGen @Int genValid shrinkValid
-- prop> encodeAndDecodeAreInversesOnGen @Int genValid shrinkValid
encodeAndDecodeAreInversesOnGen ::
  (Show a, Eq a, Binary a) => Gen a -> (a -> [a]) -> Property
encodeAndDecodeAreInversesOnGen gen s =
  forAllShrink gen s $ \(a :: a) ->
    case Binary.decodeOrFail (Binary.encode a) of
      Right (_, _, b) -> b `shouldBe` a
      Left (_, _, s_) ->
        expectationFailure $
          unwords ["decode of encode is not identity:", s_]
