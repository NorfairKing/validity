{-# LANGUAGE TypeApplications #-}

module Data.GenValidity.URISpec (spec) where

import Data.GenValidity.URI
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
      exampleSpec "mailto:John.Doe@example.com"
      exampleSpec "news:comp.infosystems.www.servers.unix"
      exampleSpec "tel:+1-816-555-1212"
      exampleSpec "telnet://192.0.2.16:80/"
      exampleSpec "urn:oasis:names:specification:docbook:dtd:xml:4.1.2"
      exampleSpec "foo://example.com:8042/over/there?name=ferret#nose"
      exampleSpec "urn:example:animal:ferret:nose"

    describe "genScheme" $ do
      it "generates schemes that are considered valid by validateScheme" $
        forAll genScheme $ \schemeCandidate ->
          case prettyValidation (validateScheme schemeCandidate) of
            Nothing -> pure ()
            Just err -> expectationFailure err

    describe "genUserInfo" $ do
      it "generates user info  that are considered valid by validateUserInfo" $
        forAll genUserInfo $ \userinfoCandidate ->
          case prettyValidation (validateUserInfo userinfoCandidate) of
            Nothing -> pure ()
            Just err -> expectationFailure err

    describe "genHost" $ do
      it "generates user info  that are considered valid by validateHost" $
        forAll genHost $ \hostCandidate ->
          case prettyValidation (validateHost hostCandidate) of
            Nothing -> pure ()
            Just err -> expectationFailure err

    describe "genPort" $ do
      it "generates user info  that are considered valid by validatePort" $
        forAll genPort $ \portCandidate ->
          case prettyValidation (validatePort portCandidate) of
            Nothing -> pure ()
            Just err -> expectationFailure err

    describe "genPath" $ do
      it "generates user info  that are considered valid by validatePath" $
        forAll genPath $ \portCandidate ->
          case prettyValidation (validatePath portCandidate) of
            Nothing -> pure ()
            Just err -> expectationFailure err

    describe "genQuery" $ do
      it "generates user info  that are considered valid by validateQuery" $
        forAll genQuery $ \portCandidate ->
          case prettyValidation (validateQuery portCandidate) of
            Nothing -> pure ()
            Just err -> expectationFailure err

    -- describe "genURI" $ do
    --   it "generates valid URI values" $
    --     genGeneratesValid genURI
    --   it "generates values that parse using parseURI" $
    --     forAll genURI $ \uri ->
    --       case parseURI (unsafeURIToString uri) of
    --         Nothing -> expectationFailure "Should have parsed."
    --         Just uri' -> uri' `shouldBe` uri

    -- describe "genURIReference" $ do
    --   it "generates valid URI values" $
    --     genGeneratesValid genURIReference
    --   it "generates values that parse using parseURIReference" $
    --     forAll genURIReference $ \uri ->
    --       case parseURIReference (unsafeURIToString uri) of
    --         Nothing -> expectationFailure "Should have parsed."
    --         Just uri' -> uri' `shouldBe` uri

    -- describe "genRelativeReference" $ do
    --   it "generates valid URI values" $
    --     genGeneratesValid genRelativeReference
    --   it "generates values that parse using parseRelativeReference" $
    --     forAll genRelativeReference $ \uri ->
    --       case parseRelativeReference (unsafeURIToString uri) of
    --         Nothing -> expectationFailure "Should have parsed."
    --         Just uri' -> uri' `shouldBe` uri

    -- describe "genAbsoluteURI" $ do
    --   it "generates valid URI values" $
    --     genGeneratesValid genAbsoluteURI
    --   it "generates values that parse using parseAbsoluteURI" $
    --     forAll genAbsoluteURI $ \uri ->
    --       case parseAbsoluteURI (unsafeURIToString uri) of
    --         Nothing -> expectationFailure "Should have parsed."
    --         Just uri' -> uri' `shouldBe` uri

    genValidSpec @URI

    it "produces URI that roundtrip through parsing" $
      forAll genValid $ \uri ->
        case parseURIReference (unsafeURIToString uri) of
          Nothing -> expectationFailure $ "Could not parse uri: " <> show (unsafeURIToString uri)
          Just uri' -> uri' `shouldBe` uri

    runIO $ sample (genValid @URI)
