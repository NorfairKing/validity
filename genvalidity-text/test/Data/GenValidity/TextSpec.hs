module Data.GenValidity.TextSpec
    ( spec
    ) where

import Control.Monad
import Data.List
import Data.Word
import Text.Printf

import Test.Hspec
import Test.QuickCheck

import Data.GenValidity
import Data.GenValidity.Text

import qualified Data.Text as T
import qualified Data.Text.Array as A
import Data.Text.Internal (Text(Text))

showTextDebug :: Text -> String
showTextDebug t@(Text arr off len) =
    unlines
        [ unwords [show t, "is invalid"]
        , unwords
              [ "arr:   "
              , intercalate "," $
                map (printf "%4d" :: Word16 -> String) $ A.toList arr off len
              ]
        , unwords
              [ "hexarr:"
              , intercalate "," $
                map (printf "%4x" :: Word16 -> String) $ A.toList arr off len
              ]
        , unwords ["off:      ", show off]
        , unwords ["len:      ", show len]
        ]

spec :: Spec
spec = do
    describe "genValid" $ do
        it "is always empty when resized to 0" $ do
            forAll (resize 0 genValid) (`shouldSatisfy` T.null)
        it "generates valid text" $
            forAll (genValid :: Gen Text) $ \t ->
                unless (isValid t) $ expectationFailure $ showTextDebug t
    describe "genUnchecked `suchThat` isValid" $
        it "generates valid text" $
        forAll ((genUnchecked :: Gen Text) `suchThat` isValid) $ \t ->
            unless (isValid t) $ expectationFailure $ showTextDebug t
    describe "textStartingWith" $ do
        it "is never empty" $ do
            forAll arbitrary $ \c ->
                forAll (textStartingWith c) $ \t -> t `shouldNotSatisfy` T.null
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
                forAll (textStartingWith c) $ \t -> T.head t `shouldBe` c
    describe "textWithA" $ do
        it "contains the given character" $ do
            forAll arbitrary $ \c ->
                forAll (textWithA c) $ \t -> T.unpack t `shouldSatisfy` (elem c)
    describe "textWithoutAny" $ do
        it "never contains the given char" $ do
            forAll arbitrary $ \c ->
                forAll (textWithoutAny c) $ \t ->
                    T.unpack t `shouldNotSatisfy` (elem c)
    describe "textWithoutAnyOf" $ do
        it "never contains any of the given chars" $ do
            forAll arbitrary $ \cs ->
                forAll (textWithoutAnyOf cs) $ \text ->
                    T.unpack text `shouldNotSatisfy` (\t -> any (`elem` t) cs)
