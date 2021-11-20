module Test.Validity.Property.Utils
  ( forAllValid,
    shouldBeValid,
    shouldBeInvalid,
    (<==>),
    (===>),
  )
where

import Data.GenValidity
import Test.Hspec
import Test.QuickCheck
import Text.Show.Pretty (ppShow)

forAllValid :: (Show a, GenValid a, Testable prop) => (a -> prop) -> Property
forAllValid = forAllShrink genValid shrinkValid

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
          [ "'validate' reported this value to be invalid: ",
            show a,
            "pretty version:",
            ppShow a,
            "with explanation:",
            err,
            ""
          ]

shouldBeInvalid :: (Show a, Validity a) => a -> Expectation
shouldBeInvalid a =
  case prettyValidate a of
    Right _ ->
      expectationFailure $
        unlines
          [ "'validate' reported this value to be valid: ",
            show a,
            "pretty version:",
            ppShow a,
            "where we expected it to be invalid"
          ]
    Left _ -> pure ()
