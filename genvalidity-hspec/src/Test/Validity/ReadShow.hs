{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Validity.ReadShow
    ( readShowSpec
    , readShowSpecOnValid
    , readShowSpecOnArbitrary
    , readShowSpecOnGen
    ) where

import Data.Typeable

import Data.GenValidity

import Text.Read

import Test.Hspec
import Test.QuickCheck

import Test.Validity.Utils

readShowSpecOnGen ::
       forall a. (Read a, Show a, Typeable a, Eq a)
    => Gen a
    -> String
    -> (a -> [a])
    -> Spec
readShowSpecOnGen gen genName shrink = do
    let name = nameOf @a
    describe ("Read " ++ name) $
        describe ("readMaybe :: String -> Maybe a") $
        it
            (concat
                 ["readMaybe . show === Just for ", genName, " \"", name, "\"s"]) $
        forAllShrink gen shrink $ \a -> (readMaybe (show a) `shouldBe` Just a)

readShowSpec ::
       forall a. (Read a, Show a, Typeable a, GenUnchecked a, Eq a)
    => Spec
readShowSpec = readShowSpecOnGen (genUnchecked @a) "unchecked" shrinkUnchecked

readShowSpecOnValid ::
       forall a. (Read a, Show a, Typeable a, GenValid a, Eq a)
    => Spec
readShowSpecOnValid = readShowSpecOnGen (genValid @a) "valid" shrinkValid

readShowSpecOnArbitrary ::
       forall a. (Read a, Show a, Typeable a, Arbitrary a, Eq a)
    => Spec
readShowSpecOnArbitrary = readShowSpecOnGen (arbitrary @a) "arbitrary" shrink
