{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Validity.Functions.Equivalence
  ( equivalentOnGen,
    equivalent,
    equivalentOnArbitrary,
    equivalentOnGens2,
    equivalent2,
    equivalentOnArbitrary2,
    equivalentWhenFirstSucceedsOnGen,
    equivalentWhenFirstSucceeds,
    equivalentWhenFirstSucceedsOnArbitrary,
    equivalentWhenFirstSucceedsOnGens2,
    equivalentWhenFirstSucceeds2,
    equivalentWhenFirstSucceedsOnArbitrary2,
    equivalentWhenSecondSucceedsOnGen,
    equivalentWhenSecondSucceeds,
    equivalentWhenSecondSucceedsOnArbitrary,
    equivalentWhenSecondSucceedsOnGens2,
    equivalentWhenSecondSucceeds2,
    equivalentWhenSecondSucceedsOnArbitrary2,
    equivalentWhenSucceedOnGen,
    equivalentWhenSucceed,
    equivalentWhenSucceedOnArbitrary,
    equivalentWhenSucceedOnGens2,
    equivalentWhenSucceed2,
    equivalentWhenSucceedOnArbitrary2,
    equivalentOnGens3,
    equivalent3,
    equivalentOnArbitrary3,
  )
where

import Data.GenValidity
import Test.Hspec
import Test.QuickCheck
import Test.Validity.Types

equivalentOnGen ::
  (Show a, Show b, Eq b) =>
  (a -> b) ->
  (a -> b) ->
  Gen a ->
  (a -> [a]) ->
  Property
equivalentOnGen f g gen s = forAllShrink gen s $ \a -> f a `shouldBe` g a

equivalent ::
  (Show a, GenValid a, Show b, Eq b) =>
  (a -> b) ->
  (a -> b) ->
  Property
equivalent f g = equivalentOnGen f g genValid shrinkValid

-- |
--
-- prop> equivalentOnArbitrary ((* 2) . (+ 1)) ((+ 2) . (* 2) :: Int -> Int)
equivalentOnArbitrary ::
  (Show a, Arbitrary a, Show b, Eq b) =>
  (a -> b) ->
  (a -> b) ->
  Property
equivalentOnArbitrary f g = equivalentOnGen f g arbitrary shrink

equivalentOnGens2 ::
  (Show a, Show b, Show c, Eq c) =>
  (a -> b -> c) ->
  (a -> b -> c) ->
  Gen (a, b) ->
  ((a, b) -> [(a, b)]) ->
  Property
equivalentOnGens2 f g gen s =
  forAllShrink gen s $ \(a, b) -> f a b `shouldBe` g a b

equivalent2 ::
  (Show a, GenValid a, Show b, GenValid b, Show c, Eq c) =>
  (a -> b -> c) ->
  (a -> b -> c) ->
  Property
equivalent2 f g = equivalentOnGens2 f g genValid shrinkValid

-- |
--
-- prop> equivalentOnArbitrary2 (+) ((+) :: Int -> Int -> Int)
equivalentOnArbitrary2 ::
  (Show a, Arbitrary a, Show b, Arbitrary b, Show c, Eq c) =>
  (a -> b -> c) ->
  (a -> b -> c) ->
  Property
equivalentOnArbitrary2 f g = equivalentOnGens2 f g arbitrary shrink

equivalentWhenFirstSucceedsOnGen ::
  (Show a, Show b, Eq b, CanFail f) =>
  (a -> f b) ->
  (a -> b) ->
  Gen a ->
  (a -> [a]) ->
  Property
equivalentWhenFirstSucceedsOnGen f g gen s =
  forAllShrink gen s $ \a ->
    case resultIfSucceeded (f a) of
      Nothing -> return () -- fine
      Just r -> r `shouldBe` g a

equivalentWhenFirstSucceedsOnArbitrary ::
  (Show a, Arbitrary a, Show b, Eq b, CanFail f) =>
  (a -> f b) ->
  (a -> b) ->
  Property
equivalentWhenFirstSucceedsOnArbitrary f g =
  equivalentWhenFirstSucceedsOnGen f g arbitrary shrink

equivalentWhenFirstSucceeds ::
  (Show a, GenValid a, Show b, Eq b, CanFail f) =>
  (a -> f b) ->
  (a -> b) ->
  Property
equivalentWhenFirstSucceeds f g =
  equivalentWhenFirstSucceedsOnGen f g genValid shrinkValid

equivalentWhenFirstSucceedsOnGens2 ::
  (Show a, Show b, Show c, Eq c, CanFail f) =>
  (a -> b -> f c) ->
  (a -> b -> c) ->
  Gen (a, b) ->
  ((a, b) -> [(a, b)]) ->
  Property
equivalentWhenFirstSucceedsOnGens2 f g gen s =
  forAllShrink gen s $ \(a, b) ->
    case resultIfSucceeded (f a b) of
      Nothing -> return () -- fine
      Just rs -> rs `shouldBe` g a b

equivalentWhenFirstSucceedsOnArbitrary2 ::
  ( Show a,
    Arbitrary a,
    Show b,
    Arbitrary b,
    Show c,
    Eq c,
    CanFail f
  ) =>
  (a -> b -> f c) ->
  (a -> b -> c) ->
  Property
equivalentWhenFirstSucceedsOnArbitrary2 f g =
  equivalentWhenFirstSucceedsOnGens2 f g arbitrary shrink

equivalentWhenFirstSucceeds2 ::
  ( Show a,
    GenValid a,
    Show b,
    GenValid b,
    Show c,
    Eq c,
    CanFail f
  ) =>
  (a -> b -> f c) ->
  (a -> b -> c) ->
  Property
equivalentWhenFirstSucceeds2 f g =
  equivalentWhenFirstSucceedsOnGens2 f g genValid shrinkValid

equivalentWhenSecondSucceedsOnGen ::
  (Show a, Show b, Eq b, CanFail f) =>
  (a -> b) ->
  (a -> f b) ->
  Gen a ->
  (a -> [a]) ->
  Property
equivalentWhenSecondSucceedsOnGen f g gen s =
  forAllShrink gen s $ \a ->
    case resultIfSucceeded (g a) of
      Nothing -> return () -- fine
      Just r -> r `shouldBe` f a

equivalentWhenSecondSucceedsOnArbitrary ::
  (Show a, Arbitrary a, Show b, Eq b, CanFail f) =>
  (a -> b) ->
  (a -> f b) ->
  Property
equivalentWhenSecondSucceedsOnArbitrary f g =
  equivalentWhenSecondSucceedsOnGen f g arbitrary shrink

equivalentWhenSecondSucceeds ::
  (Show a, GenValid a, Show b, Eq b, CanFail f) =>
  (a -> b) ->
  (a -> f b) ->
  Property
equivalentWhenSecondSucceeds f g =
  equivalentWhenSecondSucceedsOnGen f g genValid shrinkValid

equivalentWhenSecondSucceedsOnGens2 ::
  (Show a, Show b, Show c, Eq c, CanFail f) =>
  (a -> b -> c) ->
  (a -> b -> f c) ->
  Gen (a, b) ->
  ((a, b) -> [(a, b)]) ->
  Property
equivalentWhenSecondSucceedsOnGens2 f g gen s =
  forAllShrink gen s $ \(a, b) ->
    case resultIfSucceeded (g a b) of
      Nothing -> return () -- fine
      Just rs -> rs `shouldBe` f a b

equivalentWhenSecondSucceedsOnArbitrary2 ::
  ( Show a,
    Arbitrary a,
    Show b,
    Arbitrary b,
    Show c,
    Eq c,
    CanFail f
  ) =>
  (a -> b -> c) ->
  (a -> b -> f c) ->
  Property
equivalentWhenSecondSucceedsOnArbitrary2 f g =
  equivalentWhenSecondSucceedsOnGens2 f g arbitrary shrink

equivalentWhenSecondSucceeds2 ::
  ( Show a,
    GenValid a,
    Show b,
    GenValid b,
    Show c,
    Eq c,
    CanFail f
  ) =>
  (a -> b -> c) ->
  (a -> b -> f c) ->
  Property
equivalentWhenSecondSucceeds2 f g =
  equivalentWhenSecondSucceedsOnGens2 f g genValid shrinkValid

equivalentWhenSucceedOnGen ::
  (Show a, Show b, Eq b, CanFail f) =>
  (a -> f b) ->
  (a -> f b) ->
  Gen a ->
  (a -> [a]) ->
  Property
equivalentWhenSucceedOnGen f g gen s =
  forAllShrink gen s $ \a ->
    case do
      fa <- resultIfSucceeded $ f a
      ga <- resultIfSucceeded $ g a
      return (fa, ga) of
      Nothing -> return () -- fine
      Just (fa, ga) -> fa `shouldBe` ga

equivalentWhenSucceed ::
  (Show a, GenValid a, Show b, Eq b, CanFail f) =>
  (a -> f b) ->
  (a -> f b) ->
  Property
equivalentWhenSucceed f g =
  equivalentWhenSucceedOnGen f g genValid shrinkValid

equivalentWhenSucceedOnArbitrary ::
  (Show a, Arbitrary a, Show b, Eq b, CanFail f) =>
  (a -> f b) ->
  (a -> f b) ->
  Property
equivalentWhenSucceedOnArbitrary f g =
  equivalentWhenSucceedOnGen f g arbitrary shrink

equivalentWhenSucceedOnGens2 ::
  (Show a, Show b, Show c, Eq c, CanFail f) =>
  (a -> b -> f c) ->
  (a -> b -> f c) ->
  Gen (a, b) ->
  ((a, b) -> [(a, b)]) ->
  Property
equivalentWhenSucceedOnGens2 f g gen s =
  forAllShrink gen s $ \(a, b) ->
    case do
      fab <- resultIfSucceeded $ f a b
      gab <- resultIfSucceeded $ g a b
      return (fab, gab) of
      Nothing -> return () -- fine
      Just (fab, gab) -> fab `shouldBe` gab

equivalentWhenSucceedOnArbitrary2 ::
  ( Show a,
    Arbitrary a,
    Show b,
    Arbitrary b,
    Show c,
    Eq c,
    CanFail f
  ) =>
  (a -> b -> f c) ->
  (a -> b -> f c) ->
  Property
equivalentWhenSucceedOnArbitrary2 f g =
  equivalentWhenSucceedOnGens2 f g arbitrary shrink

equivalentWhenSucceed2 ::
  ( Show a,
    GenValid a,
    Show b,
    GenValid b,
    Show c,
    Eq c,
    CanFail f
  ) =>
  (a -> b -> f c) ->
  (a -> b -> f c) ->
  Property
equivalentWhenSucceed2 f g =
  equivalentWhenSucceedOnGens2 f g genValid shrinkValid

equivalentOnGens3 ::
  (Show a, Show b, Show c, Show d, Eq d) =>
  (a -> b -> c -> d) ->
  (a -> b -> c -> d) ->
  Gen (a, b, c) ->
  ((a, b, c) -> [(a, b, c)]) ->
  Property
equivalentOnGens3 f g gen s =
  forAllShrink gen s $ \(a, b, c) -> f a b c `shouldBe` g a b c

equivalent3 ::
  ( Show a,
    GenValid a,
    Show b,
    GenValid b,
    Show c,
    GenValid c,
    Show d,
    Eq d
  ) =>
  (a -> b -> c -> d) ->
  (a -> b -> c -> d) ->
  Property
equivalent3 f g = equivalentOnGens3 f g genValid shrinkValid

equivalentOnArbitrary3 ::
  ( Show a,
    Arbitrary a,
    Show b,
    Arbitrary b,
    Show c,
    Arbitrary c,
    Show d,
    Eq d
  ) =>
  (a -> b -> c -> d) ->
  (a -> b -> c -> d) ->
  Property
equivalentOnArbitrary3 f g = equivalentOnGens3 f g arbitrary shrink
