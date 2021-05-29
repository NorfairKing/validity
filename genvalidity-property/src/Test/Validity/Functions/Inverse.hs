{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Standard tests involving inverse functions
module Test.Validity.Functions.Inverse
  ( inverseFunctionsOnGen,
    inverseFunctionsOnValid,
    inverseFunctions,
    inverseFunctionsOnArbitrary,
    inverseFunctionsIfFirstSucceedsOnGen,
    inverseFunctionsIfFirstSucceedsOnValid,
    inverseFunctionsIfFirstSucceeds,
    inverseFunctionsIfFirstSucceedsOnArbitrary,
    inverseFunctionsIfSecondSucceedsOnGen,
    inverseFunctionsIfSecondSucceedsOnValid,
    inverseFunctionsIfSecondSucceeds,
    inverseFunctionsIfSecondSucceedsOnArbitrary,
    inverseFunctionsIfSucceedOnGen,
    inverseFunctionsIfSucceedOnValid,
    inverseFunctionsIfSucceed,
    inverseFunctionsIfSucceedOnArbitrary,
  )
where

import Data.GenValidity
import Test.Hspec
import Test.QuickCheck
import Test.Validity.Types

inverseFunctionsOnGen ::
  (Show a, Eq a) => (a -> b) -> (b -> a) -> Gen a -> (a -> [a]) -> Property
inverseFunctionsOnGen f g gen s =
  forAllShrink gen s $ \a -> g (f a) `shouldBe` a

inverseFunctionsOnValid ::
  (Show a, Eq a, GenValid a) => (a -> b) -> (b -> a) -> Property
inverseFunctionsOnValid f g = inverseFunctionsOnGen f g genValid shrinkValid

inverseFunctions ::
  (Show a, Eq a, GenUnchecked a) => (a -> b) -> (b -> a) -> Property
inverseFunctions f g = inverseFunctionsOnGen f g genUnchecked shrinkUnchecked

-- |
-- 'id' is its own inverse function for every type:
-- prop> inverseFunctionsOnArbitrary id (id :: Int -> Int)
inverseFunctionsOnArbitrary ::
  (Show a, Eq a, Arbitrary a) => (a -> b) -> (b -> a) -> Property
inverseFunctionsOnArbitrary f g = inverseFunctionsOnGen f g arbitrary shrink

inverseFunctionsIfFirstSucceedsOnGen ::
  (Show a, Eq a, CanFail f) =>
  (a -> f b) ->
  (b -> a) ->
  Gen a ->
  (a -> [a]) ->
  Property
inverseFunctionsIfFirstSucceedsOnGen f g gen s =
  forAllShrink gen s $ \a ->
    case resultIfSucceeded (f a) of
      Nothing -> return () -- fine
      Just b -> g b `shouldBe` a

inverseFunctionsIfFirstSucceedsOnValid ::
  (Show a, Eq a, GenValid a, CanFail f) =>
  (a -> f b) ->
  (b -> a) ->
  Property
inverseFunctionsIfFirstSucceedsOnValid f g =
  inverseFunctionsIfFirstSucceedsOnGen f g genValid shrinkValid

inverseFunctionsIfFirstSucceeds ::
  (Show a, Eq a, GenUnchecked a, CanFail f) =>
  (a -> f b) ->
  (b -> a) ->
  Property
inverseFunctionsIfFirstSucceeds f g =
  inverseFunctionsIfFirstSucceedsOnGen f g genUnchecked shrinkUnchecked

inverseFunctionsIfFirstSucceedsOnArbitrary ::
  (Show a, Eq a, Arbitrary a, CanFail f) =>
  (a -> f b) ->
  (b -> a) ->
  Property
inverseFunctionsIfFirstSucceedsOnArbitrary f g =
  inverseFunctionsIfFirstSucceedsOnGen f g arbitrary shrink

inverseFunctionsIfSecondSucceedsOnGen ::
  (Show a, Eq a, CanFail f) =>
  (a -> b) ->
  (b -> f a) ->
  Gen a ->
  (a -> [a]) ->
  Property
inverseFunctionsIfSecondSucceedsOnGen f g gen s =
  forAllShrink gen s $ \a ->
    case resultIfSucceeded (g (f a)) of
      Nothing -> return () -- fine
      Just r -> r `shouldBe` a

inverseFunctionsIfSecondSucceedsOnValid ::
  (Show a, Eq a, GenValid a, CanFail f) =>
  (a -> b) ->
  (b -> f a) ->
  Property
inverseFunctionsIfSecondSucceedsOnValid f g =
  inverseFunctionsIfSecondSucceedsOnGen f g genValid shrinkValid

inverseFunctionsIfSecondSucceeds ::
  (Show a, Eq a, GenUnchecked a, CanFail f) =>
  (a -> b) ->
  (b -> f a) ->
  Property
inverseFunctionsIfSecondSucceeds f g =
  inverseFunctionsIfSecondSucceedsOnGen f g genUnchecked shrinkUnchecked

inverseFunctionsIfSecondSucceedsOnArbitrary ::
  (Show a, Eq a, Arbitrary a, CanFail f) =>
  (a -> b) ->
  (b -> f a) ->
  Property
inverseFunctionsIfSecondSucceedsOnArbitrary f g =
  inverseFunctionsIfSecondSucceedsOnGen f g arbitrary shrink

inverseFunctionsIfSucceedOnGen ::
  (Show a, Eq a, CanFail f, CanFail g) =>
  (a -> f b) ->
  (b -> g a) ->
  Gen a ->
  (a -> [a]) ->
  Property
inverseFunctionsIfSucceedOnGen f g gen s =
  forAllShrink gen s $ \a ->
    case do
      fa <- resultIfSucceeded $ f a
      resultIfSucceeded $ g fa of
      Nothing -> return () -- fine
      Just r -> r `shouldBe` a

inverseFunctionsIfSucceedOnValid ::
  (Show a, Eq a, GenValid a, CanFail f, CanFail g) =>
  (a -> f b) ->
  (b -> g a) ->
  Property
inverseFunctionsIfSucceedOnValid f g =
  inverseFunctionsIfSucceedOnGen f g genValid shrinkValid

inverseFunctionsIfSucceed ::
  (Show a, Eq a, GenUnchecked a, CanFail f, CanFail g) =>
  (a -> f b) ->
  (b -> g a) ->
  Property
inverseFunctionsIfSucceed f g =
  inverseFunctionsIfSucceedOnGen f g genUnchecked shrinkUnchecked

inverseFunctionsIfSucceedOnArbitrary ::
  (Show a, Eq a, Arbitrary a, CanFail f, CanFail g) =>
  (a -> f b) ->
  (b -> g a) ->
  Property
inverseFunctionsIfSucceedOnArbitrary f g =
  inverseFunctionsIfSucceedOnGen f g arbitrary shrink
