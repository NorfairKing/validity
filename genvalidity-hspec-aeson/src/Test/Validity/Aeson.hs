module Test.Validity.Aeson
    ( jsonSpecOnGen
    , jsonSpecOnValid
    , jsonSpec
    , jsonSpecOnArbitrary
    , neverFailsToEncodeOnGen
    , encodeAndDecodeAreInversesOnGen
    ) where

import Data.GenValidity

import Control.DeepSeq (deepseq, force)
import Control.Exception (evaluate)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.Proxy
import Data.Typeable
import Test.Hspec
import Test.QuickCheck
import Test.Validity.Utils

jsonSpecOnGen
    :: (Show a, Eq a, Typeable a, FromJSON a, ToJSON a)
    => Proxy a -> Gen a -> String -> Spec
jsonSpecOnGen proxy gen genname = do
    let name = nameOf proxy
    describe ("JSON " ++ name ++ " (" ++ genname ++ ")") $ do
        describe ("encode :: " ++ name ++ " -> Data.ByteString.Lazy.ByteString") $
            it (unwords ["never fails to encode an", genname, name]) $
            neverFailsToEncodeOnGen proxy gen
        describe ("decode :: " ++ name ++ " -> Data.ByteString.Lazy.ByteString") $
            it
                (unwords
                     [ "ensures that encode and decode are inverses for"
                     , genname
                     , name ++ "'s"
                     ]) $
            encodeAndDecodeAreInversesOnGen proxy gen

jsonSpecOnValid
    :: (Show a, Eq a, Typeable a, GenValid a, FromJSON a, ToJSON a)
    => Proxy a -> Spec
jsonSpecOnValid proxy = do
    jsonSpecOnGen proxy genValid "valid"

jsonSpec
    :: (Show a, Eq a, Typeable a, GenUnchecked a, FromJSON a, ToJSON a)
    => Proxy a -> Spec
jsonSpec proxy = do
    jsonSpecOnGen proxy genUnchecked "unchecked"

jsonSpecOnArbitrary
    :: (Show a, Eq a, Typeable a, Arbitrary a, FromJSON a, ToJSON a)
    => Proxy a -> Spec
jsonSpecOnArbitrary proxy = do
    jsonSpecOnGen proxy arbitrary "arbitrary"

neverFailsToEncodeOnGen
    :: (Show a, ToJSON a)
    => Proxy a -> Gen a -> Property
neverFailsToEncodeOnGen proxy gen =
    forAll gen $ \a ->
        evaluate (deepseq (JSON.encode (a `asProxyTypeOf` proxy)) ()) `shouldReturn`
        ()

encodeAndDecodeAreInversesOnGen
    :: (Show a, Eq a, FromJSON a, ToJSON a)
    => Proxy a -> Gen a -> Property
encodeAndDecodeAreInversesOnGen proxy gen =
    forAll gen $ \a ->
        JSON.eitherDecode (JSON.encode (a `asProxyTypeOf` proxy)) `shouldBe`
        Right a
