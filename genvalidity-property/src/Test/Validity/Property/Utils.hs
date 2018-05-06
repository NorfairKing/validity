module Test.Validity.Property.Utils
    ( forAllUnchecked
    , forAllValid
    , forAllInvalid
    , shouldBeValid
    , shouldBeInvalid
    , (<==>)
    , (===>)
    ) where

import Control.Monad (unless)

import Test.Hspec
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

shouldBeValid :: (Show a, Validity a) => a -> Expectation
shouldBeValid a = do
    case prettyValidation a of
        Right _ -> pure ()
        Left err ->
            expectationFailure $
            unlines
                [ "'validate' reported this value to be invalid: " ++ show a
                , err
                , ""
                ]
    unless (isValid a) $
        expectationFailure $
        unlines
            [ "isValid considered this value invalid: " ++ show a
            , "This is odd because 'validate' reported no issues."
            , "Are you sure 'Validity' is implemented correctly?"
            ]

shouldBeInvalid :: (Show a, Validity a) => a -> Expectation
shouldBeInvalid a = a `shouldNotSatisfy` isValid
