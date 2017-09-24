module Test.Validity.Property.Utils
    ( forAllUnchecked
    , forAllValid
    , forAllInvalid
    , (<==>)
    , (===>)
    ) where

import Test.QuickCheck

import Data.GenValidity

forAllUnchecked ::
       (Show a, GenUnchecked a, Testable prop) => (a -> prop) -> Property
forAllUnchecked = forAllShrink genUnchecked shrinkUnchecked

forAllValid :: (Show a, GenValid a, Testable prop) => (a -> prop) -> Property
forAllValid = forAllShrink genValid shrinkValid

forAllInvalid ::
       (Show a, GenInvalid a, Testable prop) => (a -> prop) -> Property
forAllInvalid = forAllShrink genInvalid shrinkInvalid

(===>) :: Bool -> Bool -> Bool
(===>) a b = not a || b

(<==>) :: Bool -> Bool -> Bool
(<==>) a b = a ===> b && b ===> a
