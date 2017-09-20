{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Standard test `Spec`s and raw `Property`s for `Serialize` instances.
--
-- You will need @TypeApplications@ to use these.
module Test.Validity.Cereal
    ( serializeSpecOnValid
    , serializeSpec
    , serializeSpecOnArbitrary
    , serializeSpecOnGen
    , neverFailsToEncodeOnGen
    , encodeAndDecodeAreInversesOnGen
    ) where

import Data.GenValidity

import Control.DeepSeq (deepseq)
import Control.Exception (evaluate)
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import Data.Typeable
import Test.Hspec
import Test.QuickCheck
import Test.Validity.Utils

-- | Standard test spec for properties of 'Serialize'-related functions for valid values
--
-- Example usage:
--
-- > serializeSpecOnValid @Double
serializeSpecOnValid ::
       forall a. (Show a, Eq a, Typeable a, GenValid a, Serialize a)
    => Spec
serializeSpecOnValid = serializeSpecOnGen (genValid @a) "valid" shrinkValid

-- | Standard test spec for properties of 'Serialize'-related functions for unchecked values
--
-- Example usage:
--
-- > serializeSpec @Int
serializeSpec ::
       forall a. (Show a, Eq a, Typeable a, GenUnchecked a, Serialize a)
    => Spec
serializeSpec = serializeSpecOnGen (genUnchecked @a) "unchecked" shrinkUnchecked

-- | Standard test spec for properties of 'Serialize'-related functions for arbitrary values
--
-- Example usage:
--
-- > serializeSpecOnArbitrary @Int
serializeSpecOnArbitrary ::
       forall a. (Show a, Eq a, Typeable a, Arbitrary a, Serialize a)
    => Spec
serializeSpecOnArbitrary = serializeSpecOnGen (arbitrary @a) "arbitrary" shrink

-- | Standard test spec for properties of 'Serialize'-related functions for a given generator (and a name for that generator).
--
-- Example usage:
--
-- > serializeSpecOnGen (genListOf $ pure 'a') "sequence of 'a's"
serializeSpecOnGen ::
       forall a. (Show a, Eq a, Typeable a, Serialize a)
    => Gen a
    -> String
    -> (a -> [a])
    -> Spec
serializeSpecOnGen gen genname s =
    parallel $ do
        let name = nameOf @a
        describe ("Serialize " ++ name ++ " (" ++ genname ++ ")") $ do
            describe ("encode :: " ++ name ++ " -> Data.ByteString.ByteString") $
                it
                    (unwords
                         [ "never fails to encode a"
                         , "\"" ++ genname
                         , name ++ "\""
                         ]) $
                neverFailsToEncodeOnGen gen s
            describe ("decode :: " ++ name ++ " -> Data.ByteString.ByteString") $
                it
                    (unwords
                         [ "ensures that encode and decode are inverses for"
                         , "\"" ++ genname
                         , name ++ "\"" ++ "'s"
                         ]) $
                encodeAndDecodeAreInversesOnGen gen s

-- |
--
-- prop> neverFailsToEncodeOnGen @Bool arbitrary shrink
-- prop> neverFailsToEncodeOnGen @Bool genUnchecked shrinkUnchecked
-- prop> neverFailsToEncodeOnGen @Bool genValid shrinkValid
-- prop> neverFailsToEncodeOnGen @Int arbitrary shrink
-- prop> neverFailsToEncodeOnGen @Int genUnchecked shrinkUnchecked
-- prop> neverFailsToEncodeOnGen @Int genValid shrinkValid
neverFailsToEncodeOnGen ::
       (Show a, Serialize a) => Gen a -> (a -> [a]) -> Property
neverFailsToEncodeOnGen gen s =
    forAllShrink gen s $ \(a :: a) ->
        evaluate (deepseq (Serialize.encode a) ()) `shouldReturn` ()

-- |
--
-- prop> encodeAndDecodeAreInversesOnGen @Bool arbitrary shrink
-- prop> encodeAndDecodeAreInversesOnGen @Bool genUnchecked shrinkUnchecked
-- prop> encodeAndDecodeAreInversesOnGen @Bool genValid shrinkValid
-- prop> encodeAndDecodeAreInversesOnGen @Int arbitrary shrink
-- prop> encodeAndDecodeAreInversesOnGen @Int genUnchecked shrinkUnchecked
-- prop> encodeAndDecodeAreInversesOnGen @Int genValid shrinkValid
encodeAndDecodeAreInversesOnGen ::
       (Show a, Eq a, Serialize a) => Gen a -> (a -> [a]) -> Property
encodeAndDecodeAreInversesOnGen gen s =
    forAllShrink gen s $ \(a :: a) ->
        Serialize.decode (Serialize.encode a) `shouldBe` Right a
