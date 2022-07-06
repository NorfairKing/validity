{-# LANGUAGE TypeApplications #-}

module Data.GenValidity.URISpec (spec) where

import Data.GenValidity.URI ()
import Data.Validity.URI
import Network.URI
import Test.Hspec
import Test.QuickCheck
import Test.Validity

spec :: Spec
spec = do
  describe "URIAuth" $ do
    genValidSpec @URIAuth

  describe "URI" $ do
    describe "considers these examples from the spec valid" $ do
      let exampleSpec string = it ("Considers valid: " <> show string) $
            case parseURIReference string of
              Nothing -> expectationFailure "should not happen."
              Just uri -> shouldBeValid uri

      exampleSpec "ftp://ftp.is.co.za/rfc/rfc1808.txt"
      exampleSpec "http://www.ietf.org/rfc/rfc2396.txt"
      exampleSpec "ldap://[2001:db8::7]/c=GB?objectClass?one"
      xdescribe "fails" $ exampleSpec "mailto:John.Doe@example.com"
      xdescribe "fails" $ exampleSpec "news:comp.infosystems.www.servers.unix"
      xdescribe "fails" $ exampleSpec "tel:+1-816-555-1212"
      exampleSpec "telnet://192.0.2.16:80/"
      xdescribe "fails" $ exampleSpec "urn:oasis:names:specification:docbook:dtd:xml:4.1.2"

    genValidSpec @URI

    it "produces URI that roundtrip through parsing" $
      forAll genValid $ \uri ->
        case parseURI (unsafeURIToString uri) of
          Nothing -> expectationFailure $ "Could not parse uri: " <> show (unsafeURIToString uri)
          Just uri' -> uri' `shouldBe` uri
