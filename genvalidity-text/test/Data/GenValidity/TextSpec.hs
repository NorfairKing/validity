{-# LANGUAGE TypeApplications #-}

module Data.GenValidity.TextSpec
  ( spec,
  )
where

import Control.Monad
import Data.GenValidity.Text
import Data.List
import qualified Data.Text as ST
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as ST
import qualified Data.Text.Internal.Lazy as LT
import qualified Data.Text.Lazy as LT
import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Text.Printf

showTextDebug :: ST.Text -> String
showTextDebug (ST.Text arr off len) =
  unlines
    [ unwords
        [ "arr:   ",
          intercalate "," $
            map (printf "%4d") $ A.toList arr off len
        ],
      unwords
        [ "hexarr:",
          intercalate "," $
            map (printf "%4x") $ A.toList arr off len
        ],
      unwords ["off:      ", show off],
      unwords ["len:      ", show len]
    ]

showLazyTextDebug :: LT.Text -> String
showLazyTextDebug LT.Empty = "empty"
showLazyTextDebug (LT.Chunk st lt) =
  unlines
    [ "A chuck with this strict text:",
      showTextDebug st,
      "and this rest of the lazy text:",
      showLazyTextDebug lt,
      ""
    ]

spec :: Spec
spec = do
  describe "Strict Text" $ do
    genValidSpec @ST.Text
    describe "genValid" $ do
      it "is always empty when resized to 0" $
        forAll (resize 0 genValid) (`shouldSatisfy` ST.null)
      it "generates valid text" $
        forAll (genValid) $ \t ->
          unless (isValid t) $ expectationFailure $ showTextDebug t
    describe "textStartingWith" $ do
      it "is never empty" $
        forAll arbitrary $ \c ->
          forAll (textStartingWith c) $ \t ->
            t `shouldNotSatisfy` ST.null
      it "contains exactly the first character if resized to 0" $
        forAll arbitrary $ \c ->
          forAll (resize 0 $ textStartingWith c) $ \t ->
            t `shouldBe` ST.pack [c]
      it "contains exactly the first character if resized to 1" $
        forAll arbitrary $ \c ->
          forAll (resize 0 $ textStartingWith c) $ \t ->
            t `shouldBe` ST.pack [c]
      it "always starts with the given char" $
        forAll arbitrary $ \c ->
          forAll (textStartingWith c) $ \t -> ST.head t `shouldBe` c
    describe "textWithA" $
      it "contains the given character" $
        forAll arbitrary $ \c ->
          forAll (textWithA c) $ \t -> ST.unpack t `shouldSatisfy` elem c
    describe "textWithoutAny" $ do
      it "works with \65533" $
        let c = '\65533'
         in forAll (textWithoutAny c) $ \t ->
              ST.unpack t `shouldNotSatisfy` elem c
      it "never contains the given char" $
        forAllShrink arbitrary shrink $ \c ->
          forAll (textWithoutAny c) $ \t ->
            ST.unpack t `shouldNotSatisfy` elem c
    describe "textWithoutAnyOf" $ do
      it "works with \65533" $
        let cs = "\65533"
         in forAll (textWithoutAnyOf cs) $ \text ->
              ST.unpack text `shouldNotSatisfy` (\t -> any (`elem` t) cs)
      it "never contains any of the given chars" $
        forAllShrink arbitrary shrink $ \cs ->
          forAll (textWithoutAnyOf cs) $ \text ->
            ST.unpack text `shouldNotSatisfy` (\t -> any (`elem` t) cs)
  describe "Lazy Text" $ do
    genValidSpec @LT.Text
    describe "genValid" $ do
      it "is always empty when resized to 0" $
        forAll (resize 0 genValid) (`shouldSatisfy` LT.null)
      it "generates valid text" $
        forAll (genValid) $ \t ->
          unless (isValid t) $
            expectationFailure $ showLazyTextDebug t
