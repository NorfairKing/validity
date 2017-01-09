{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Validity.Functions.Equivalence
    ( equivalentOnGen
    , equivalentOnValid
    , equivalent
    , equivalentOnArbitrary
    , equivalentOnGens2
    , equivalentOnValids2
    , equivalent2
    , equivalentOnArbitrary2
    , equivalentWhenFirstSucceedsOnGen
    , equivalentWhenFirstSucceedsOnValid
    , equivalentWhenFirstSucceeds
    , equivalentWhenFirstSucceedsOnArbitrary
    , equivalentWhenFirstSucceedsOnGens2
    , equivalentWhenFirstSucceedsOnValids2
    , equivalentWhenFirstSucceeds2
    , equivalentWhenFirstSucceedsOnArbitrary2
    , equivalentWhenSecondSucceedsOnGen
    , equivalentWhenSecondSucceedsOnValid
    , equivalentWhenSecondSucceeds
    , equivalentWhenSecondSucceedsOnArbitrary
    , equivalentWhenSecondSucceedsOnGens2
    , equivalentWhenSecondSucceedsOnValids2
    , equivalentWhenSecondSucceeds2
    , equivalentWhenSecondSucceedsOnArbitrary2
    , equivalentWhenSucceedOnGen
    , equivalentWhenSucceedOnValid
    , equivalentWhenSucceed
    , equivalentWhenSucceedOnArbitrary
    , equivalentWhenSucceedOnGens2
    , equivalentWhenSucceedOnValids2
    , equivalentWhenSucceed2
    , equivalentWhenSucceedOnArbitrary2
    ) where

import Data.GenValidity

import Test.Hspec
import Test.QuickCheck

import Test.Validity.Types

equivalentOnGen
    :: (Show a, Eq a, Show b, Eq b)
    => (a -> b) -> (a -> b) -> Gen a -> Property
equivalentOnGen f g gen = forAll gen $ \a -> f a `shouldBe` g a

equivalentOnValid
    :: (Show a, Eq a, GenValid a, Show b, Eq b)
    => (a -> b) -> (a -> b) -> Property
equivalentOnValid f g = equivalentOnGen f g genValid

equivalent
    :: (Show a, Eq a, GenUnchecked a, Show b, Eq b)
    => (a -> b) -> (a -> b) -> Property
equivalent f g = equivalentOnGen f g genUnchecked

-- |
--
-- prop> equivalentOnArbitrary ((* 2) . (+ 1)) ((+ 2) . (* 2) :: Int -> Int)
equivalentOnArbitrary
    :: (Show a, Eq a, Arbitrary a, Show b, Eq b)
    => (a -> b) -> (a -> b) -> Property
equivalentOnArbitrary f g = equivalentOnGen f g arbitrary

equivalentOnGens2
    :: (Show a, Eq a, Show b, Eq b, Show c, Eq c)
    => (a -> b -> c) -> (a -> b -> c) -> Gen (a, b) -> Property
equivalentOnGens2 f g gen = forAll gen $ \(a, b) -> f a b `shouldBe` g a b

equivalentOnValids2
    :: (Show a, Eq a, GenValid a, Show b, Eq b, GenValid b, Show c, Eq c)
    => (a -> b -> c) -> (a -> b -> c) -> Property
equivalentOnValids2 f g = equivalentOnGens2 f g genValid

equivalent2
    :: ( Show a
       , Eq a
       , GenUnchecked a
       , Show b
       , Eq b
       , GenUnchecked b
       , Show c
       , Eq c
       )
    => (a -> b -> c) -> (a -> b -> c) -> Property
equivalent2 f g = equivalentOnGens2 f g genUnchecked

-- |
--
-- prop> equivalentOnArbitrary2 (+) ((+) :: Int -> Int -> Int)
equivalentOnArbitrary2
    :: (Show a, Eq a, Arbitrary a, Show b, Eq b, Arbitrary b, Show c, Eq c)
    => (a -> b -> c) -> (a -> b -> c) -> Property
equivalentOnArbitrary2 f g = equivalentOnGens2 f g arbitrary

equivalentWhenFirstSucceedsOnGen
    :: (Show a, Eq a, Show b, Eq b, CanFail f)
    => (a -> f b) -> (a -> b) -> Gen a -> Property
equivalentWhenFirstSucceedsOnGen f g gen =
    forAll gen $ \a ->
        case resultIfSucceeded (f a) of
            Nothing -> return () -- fine
            Just r -> r `shouldBe` g a

equivalentWhenFirstSucceedsOnValid
    :: (Show a, Eq a, GenValid a, Show b, Eq b, CanFail f)
    => (a -> f b) -> (a -> b) -> Property
equivalentWhenFirstSucceedsOnValid f g =
    equivalentWhenFirstSucceedsOnGen f g genValid

equivalentWhenFirstSucceedsOnArbitrary
    :: (Show a, Eq a, Arbitrary a, Show b, Eq b, CanFail f)
    => (a -> f b) -> (a -> b) -> Property
equivalentWhenFirstSucceedsOnArbitrary f g =
    equivalentWhenFirstSucceedsOnGen f g arbitrary

equivalentWhenFirstSucceeds
    :: (Show a, Eq a, GenUnchecked a, Show b, Eq b, CanFail f)
    => (a -> f b) -> (a -> b) -> Property
equivalentWhenFirstSucceeds f g =
    equivalentWhenFirstSucceedsOnGen f g genUnchecked

equivalentWhenFirstSucceedsOnGens2
    :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, CanFail f)
    => (a -> b -> f c) -> (a -> b -> c) -> Gen (a, b) -> Property
equivalentWhenFirstSucceedsOnGens2 f g gen =
    forAll gen $ \(a, b) ->
        case resultIfSucceeded (f a b) of
            Nothing -> return () -- fine
            Just rs -> rs `shouldBe` g a b

equivalentWhenFirstSucceedsOnValids2
    :: ( Show a
       , Eq a
       , GenValid a
       , Show b
       , Eq b
       , GenValid b
       , Show c
       , Eq c
       , CanFail f
       )
    => (a -> b -> f c) -> (a -> b -> c) -> Property
equivalentWhenFirstSucceedsOnValids2 f g =
    equivalentWhenFirstSucceedsOnGens2 f g genValid

equivalentWhenFirstSucceedsOnArbitrary2
    :: ( Show a
       , Eq a
       , Arbitrary a
       , Show b
       , Eq b
       , Arbitrary b
       , Show c
       , Eq c
       , CanFail f
       )
    => (a -> b -> f c) -> (a -> b -> c) -> Property
equivalentWhenFirstSucceedsOnArbitrary2 f g =
    equivalentWhenFirstSucceedsOnGens2 f g arbitrary

equivalentWhenFirstSucceeds2
    :: ( Show a
       , Eq a
       , GenUnchecked a
       , Show b
       , Eq b
       , GenUnchecked b
       , Show c
       , Eq c
       , CanFail f
       )
    => (a -> b -> f c) -> (a -> b -> c) -> Property
equivalentWhenFirstSucceeds2 f g =
    equivalentWhenFirstSucceedsOnGens2 f g genUnchecked

equivalentWhenSecondSucceedsOnGen
    :: (Show a, Eq a, Show b, Eq b, CanFail f)
    => (a -> b) -> (a -> f b) -> Gen a -> Property
equivalentWhenSecondSucceedsOnGen f g gen =
    forAll gen $ \a ->
        case resultIfSucceeded (g a) of
            Nothing -> return () -- fine
            Just r -> r `shouldBe` f a

equivalentWhenSecondSucceedsOnValid
    :: (Show a, Eq a, GenValid a, Show b, Eq b, CanFail f)
    => (a -> b) -> (a -> f b) -> Property
equivalentWhenSecondSucceedsOnValid f g =
    equivalentWhenSecondSucceedsOnGen f g genValid

equivalentWhenSecondSucceedsOnArbitrary
    :: (Show a, Eq a, Arbitrary a, Show b, Eq b, CanFail f)
    => (a -> b) -> (a -> f b) -> Property
equivalentWhenSecondSucceedsOnArbitrary f g =
    equivalentWhenSecondSucceedsOnGen f g arbitrary

equivalentWhenSecondSucceeds
    :: (Show a, Eq a, GenUnchecked a, Show b, Eq b, CanFail f)
    => (a -> b) -> (a -> f b) -> Property
equivalentWhenSecondSucceeds f g =
    equivalentWhenSecondSucceedsOnGen f g genUnchecked

equivalentWhenSecondSucceedsOnGens2
    :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, CanFail f)
    => (a -> b -> c) -> (a -> b -> f c) -> Gen (a, b) -> Property
equivalentWhenSecondSucceedsOnGens2 f g gen =
    forAll gen $ \(a, b) ->
        case resultIfSucceeded (g a b) of
            Nothing -> return () -- fine
            Just rs -> rs `shouldBe` f a b

equivalentWhenSecondSucceedsOnValids2
    :: ( Show a
       , Eq a
       , GenValid a
       , Show b
       , Eq b
       , GenValid b
       , Show c
       , Eq c
       , CanFail f
       )
    => (a -> b -> c) -> (a -> b -> f c) -> Property
equivalentWhenSecondSucceedsOnValids2 f g =
    equivalentWhenSecondSucceedsOnGens2 f g genValid

equivalentWhenSecondSucceedsOnArbitrary2
    :: ( Show a
       , Eq a
       , Arbitrary a
       , Show b
       , Eq b
       , Arbitrary b
       , Show c
       , Eq c
       , CanFail f
       )
    => (a -> b -> c) -> (a -> b -> f c) -> Property
equivalentWhenSecondSucceedsOnArbitrary2 f g =
    equivalentWhenSecondSucceedsOnGens2 f g arbitrary

equivalentWhenSecondSucceeds2
    :: ( Show a
       , Eq a
       , GenUnchecked a
       , Show b
       , Eq b
       , GenUnchecked b
       , Show c
       , Eq c
       , CanFail f
       )
    => (a -> b -> c) -> (a -> b -> f c) -> Property
equivalentWhenSecondSucceeds2 f g =
    equivalentWhenSecondSucceedsOnGens2 f g genUnchecked

equivalentWhenSucceedOnGen
    :: (Show a, Eq a, Show b, Eq b, CanFail f)
    => (a -> f b) -> (a -> f b) -> Gen a -> Property
equivalentWhenSucceedOnGen f g gen =
    forAll gen $ \a ->
        case do fa <- resultIfSucceeded $ f a
                ga <- resultIfSucceeded $ g a
                return (fa, ga) of
            Nothing -> return () -- fine
            Just (fa, ga) -> fa `shouldBe` ga

equivalentWhenSucceedOnValid
    :: (Show a, Eq a, GenValid a, Show b, Eq b, CanFail f)
    => (a -> f b) -> (a -> f b) -> Property
equivalentWhenSucceedOnValid f g = equivalentWhenSucceedOnGen f g genValid

equivalentWhenSucceed
    :: (Show a, Eq a, GenUnchecked a, Show b, Eq b, CanFail f)
    => (a -> f b) -> (a -> f b) -> Property
equivalentWhenSucceed f g = equivalentWhenSucceedOnGen f g genUnchecked

equivalentWhenSucceedOnArbitrary
    :: (Show a, Eq a, Arbitrary a, Show b, Eq b, CanFail f)
    => (a -> f b) -> (a -> f b) -> Property
equivalentWhenSucceedOnArbitrary f g = equivalentWhenSucceedOnGen f g arbitrary

equivalentWhenSucceedOnGens2
    :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, CanFail f)
    => (a -> b -> f c) -> (a -> b -> f c) -> Gen (a, b) -> Property
equivalentWhenSucceedOnGens2 f g gen =
    forAll gen $ \(a, b) ->
        case do fab <- resultIfSucceeded $ f a b
                gab <- resultIfSucceeded $ g a b
                return (fab, gab) of
            Nothing -> return () -- fine
            Just (fab, gab) -> fab `shouldBe` gab

equivalentWhenSucceedOnValids2
    :: ( Show a
       , Eq a
       , GenValid a
       , Show b
       , Eq b
       , GenValid b
       , Show c
       , Eq c
       , CanFail f
       )
    => (a -> b -> f c) -> (a -> b -> f c) -> Property
equivalentWhenSucceedOnValids2 f g = equivalentWhenSucceedOnGens2 f g genValid

equivalentWhenSucceedOnArbitrary2
    :: ( Show a
       , Eq a
       , Arbitrary a
       , Show b
       , Eq b
       , Arbitrary b
       , Show c
       , Eq c
       , CanFail f
       )
    => (a -> b -> f c) -> (a -> b -> f c) -> Property
equivalentWhenSucceedOnArbitrary2 f g =
    equivalentWhenSucceedOnGens2 f g arbitrary

equivalentWhenSucceed2
    :: ( Show a
       , Eq a
       , GenUnchecked a
       , Show b
       , Eq b
       , GenUnchecked b
       , Show c
       , Eq c
       , CanFail f
       )
    => (a -> b -> f c) -> (a -> b -> f c) -> Property
equivalentWhenSucceed2 f g = equivalentWhenSucceedOnGens2 f g genUnchecked
