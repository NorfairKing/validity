{-# LANGUAGE CPP #-}

module Test.Validity.Property.Utils
    ( forAllUnchecked
    , forAllValid
    , forAllInvalid
    , shouldBeValid
    , shouldBeInvalid
    , (<==>)
    , (===>)
    ) where

import Data.GenValidity
import Test.Hspec
import Test.QuickCheck

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure)
#endif

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

shouldBeValid :: (Show a, Validity a) => a -> Expectation
shouldBeValid a =
    case prettyValidate a of
        Right _ -> pure ()
        Left err ->
            expectationFailure $
            unlines
                [ "'validate' reported this value to be invalid: "
                , show a
                , "with explanation"
                , err
                , ""
                ]

shouldBeInvalid :: (Show a, Validity a) => a -> Expectation
shouldBeInvalid a =
    case prettyValidate a of
        Right _ ->
            expectationFailure $
            unlines
                [ "'validate' reported this value to be valid: "
                , show a
                , "where we expected it to be invalid"
                ]
        Left _ -> pure ()
