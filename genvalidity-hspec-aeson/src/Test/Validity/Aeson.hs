module Test.Validity.Aeson
    ( jsonSpec
    , jsonSpecOnArbitrary
    , neverFailsToEncodeOnGen
    , encodeAndDecodeAreInversesOnGen
    ) where

import           Data.GenValidity

import           Control.DeepSeq     (deepseq)
import           Control.Exception   (evaluate)
import           Data.Aeson          (FromJSON, ToJSON)
import qualified Data.Aeson          as JSON
import           Data.Proxy
import           Data.Typeable
import           Test.Hspec
import           Test.QuickCheck
import           Test.Validity.Utils

jsonSpec :: (Show a, Eq a, Typeable a, GenValidity a, FromJSON a, ToJSON a) => Proxy a -> Spec
jsonSpec proxy = do
    let name = nameOf proxy
    describe ("JSON " ++ name) $ do
        it ("never fails to encode a valid " ++ name) $
            neverFailsToEncodeOnGen proxy genValid

        it ("ensures that encode and decode are inverses for valid " ++ name ++ "'s") $
            encodeAndDecodeAreInversesOnGen proxy genValid


jsonSpecOnArbitrary :: (Show a, Eq a, Typeable a, Arbitrary a, FromJSON a, ToJSON a) => Proxy a -> Spec
jsonSpecOnArbitrary proxy = do
    let name = nameOf proxy
    describe ("JSON " ++ name) $ do
        it ("never fails to encode an arbitrary " ++ name) $
            neverFailsToEncodeOnGen proxy arbitrary

        it ("ensures that encode and decode are inverses for arbitrary " ++ name ++ "'s") $
            encodeAndDecodeAreInversesOnGen proxy arbitrary


neverFailsToEncodeOnGen :: (Show a, ToJSON a) => Proxy a -> Gen a -> Property
neverFailsToEncodeOnGen proxy gen =
    forAll gen $ \a ->
        evaluate (deepseq (JSON.encode (a `asProxyTypeOf` proxy)) ())
            `shouldReturn` ()

encodeAndDecodeAreInversesOnGen :: (Show a, Eq a, FromJSON a, ToJSON a) => Proxy a -> Gen a -> Property
encodeAndDecodeAreInversesOnGen proxy gen =
    forAll gen $ \a ->
        JSON.eitherDecode (JSON.encode (a `asProxyTypeOf` proxy))
            `shouldBe` Right a

