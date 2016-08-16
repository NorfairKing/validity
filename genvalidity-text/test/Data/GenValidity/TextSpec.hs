module Data.GenValidity.TextSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.GenValidity
import           Data.GenValidity.Text

import qualified Data.Text             as T

spec :: Spec
spec = do
    describe "genValid" $ do
        it "is always empty when resized to 0" $ do
            forAll (resize 0 genValid) (`shouldSatisfy` T.null)

    describe "textStartingWith" $ do
        it "is never empty" $ do
            forAll arbitrary $ \c ->
                forAll (textStartingWith c) $ \t ->
                    t `shouldNotSatisfy` T.null

        it "contains exactly the first character if resized to 0" $ do
            forAll arbitrary $ \c ->
                forAll (resize 0 $ textStartingWith c) $ \t ->
                    t `shouldBe` T.pack [c]

        it "contains exactly the first character if resized to 1" $ do
            forAll arbitrary $ \c ->
                forAll (resize 0 $ textStartingWith c) $ \t ->
                    t `shouldBe` T.pack [c]

        it "always starts with the given char" $ do
            forAll arbitrary $ \c ->
                forAll (textStartingWith c) $ \t ->
                    T.head t `shouldBe` c

    describe "textWithA" $ do
        it "contains the given character" $ do
            forAll arbitrary $ \c ->
                forAll (textWithA c) $ \t ->
                    T.unpack t `shouldSatisfy` (elem c)

    describe "textWithoutAny" $ do
        it "never contains the given char" $ do
            forAll arbitrary $ \c ->
                forAll (textWithoutAny c) $ \t ->
                    T.unpack t `shouldNotSatisfy` (elem c)

    describe "textWithoutAnyOf" $ do
        it "never contains any of the given chars" $ do
            forAll arbitrary $ \cs ->
                forAll (textWithoutAnyOf cs) $ \t ->
                    T.unpack t `shouldNotSatisfy` (\t -> any (`elem` t) cs)

