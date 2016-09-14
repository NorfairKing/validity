{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.Functions.Equivalence
    ( -- ** Standard tests involving equivalence of functions
      equivalentOnGen
    , equivalentOnValid
    , equivalent
    , equivalentOnGens2
    , equivalentOnValids2
    , equivalent2
    , equivalentWhenFirstSucceedsOnGen
    , equivalentWhenFirstSucceedsOnValid
    , equivalentWhenFirstSucceeds
    , equivalentWhenFirstSucceedsOnGens2
    , equivalentWhenFirstSucceedsOnValids2
    , equivalentWhenFirstSucceeds2
    , equivalentWhenSecondSucceedsOnGen
    , equivalentWhenSecondSucceedsOnValid
    , equivalentWhenSecondSucceeds
    , equivalentWhenSecondSucceedsOnGens2
    , equivalentWhenSecondSucceedsOnValids2
    , equivalentWhenSecondSucceeds2
    , equivalentWhenSucceedOnGen
    , equivalentWhenSucceedOnValid
    , equivalentWhenSucceed
    , equivalentWhenSucceedOnGens2
    , equivalentWhenSucceedOnValids2
    , equivalentWhenSucceed2
    ) where

import           Data.GenValidity

import           Test.Hspec
import           Test.QuickCheck

import           Test.Validity.Types

equivalentOnGen
    :: (Show a, Eq a, Show b, Eq b)
    => (a -> b)
    -> (a -> b)
    -> Gen a
    -> Property
equivalentOnGen f g gen =
    forAll gen $ \a ->
        f a `shouldBe` g a

equivalentOnValid
    :: (Show a, Eq a, GenValidity a, Show b, Eq b)
    => (a -> b)
    -> (a -> b)
    -> Property
equivalentOnValid f g
    = equivalentOnGen f g genValid

equivalent
    :: (Show a, Eq a, GenValidity a, Show b, Eq b)
    => (a -> b)
    -> (a -> b)
    -> Property
equivalent f g
    = equivalentOnGen f g genUnchecked

equivalentOnGens2
    :: (Show a, Eq a,
        Show b, Eq b,
        Show c, Eq c)
    => (a -> b -> c)
    -> (a -> b -> c)
    -> Gen (a, b)
    -> Property
equivalentOnGens2 f g gen =
    forAll gen $ \(a, b) ->
        f a b `shouldBe` g a b

equivalentOnValids2
    :: (Show a, Eq a, GenValidity a,
        Show b, Eq b, GenValidity b,
        Show c, Eq c)
    => (a -> b -> c)
    -> (a -> b -> c)
    -> Property
equivalentOnValids2 f g
    = equivalentOnGens2 f g genValid

equivalent2
    :: (Show a, Eq a, GenValidity a,
        Show b, Eq b, GenValidity b,
        Show c, Eq c)
    => (a -> b -> c)
    -> (a -> b -> c)
    -> Property
equivalent2 f g
    = equivalentOnGens2 f g genUnchecked

equivalentWhenFirstSucceedsOnGen
    :: (Show a, Eq a, Show b, Eq b, CanFail f)
    => (a -> f b)
    -> (a -> b)
    -> Gen a
    -> Property
equivalentWhenFirstSucceedsOnGen f g gen =
    forAll gen $ \a ->
        case resultIfSucceeded (f a) of
            Nothing -> return () -- fine
            Just r  -> r `shouldBe` g a

equivalentWhenFirstSucceedsOnValid
    :: (Show a, Eq a, GenValidity a, Show b, Eq b, CanFail f)
    => (a -> f b)
    -> (a -> b)
    -> Property
equivalentWhenFirstSucceedsOnValid f g
    = equivalentWhenFirstSucceedsOnGen f g genValid

equivalentWhenFirstSucceeds
    :: (Show a, Eq a, GenValidity a, Show b, Eq b, CanFail f)
    => (a -> f b)
    -> (a -> b)
    -> Property
equivalentWhenFirstSucceeds f g
    = equivalentWhenFirstSucceedsOnGen f g genUnchecked

equivalentWhenFirstSucceedsOnGens2
    :: (Show a, Eq a,
        Show b, Eq b,
        Show c, Eq c,
        CanFail f)
    => (a -> b -> f c)
    -> (a -> b -> c)
    -> Gen (a, b)
    -> Property
equivalentWhenFirstSucceedsOnGens2 f g gen =
    forAll gen $ \(a, b) ->
        case resultIfSucceeded (f a b) of
            Nothing -> return () -- fine
            Just rs -> rs `shouldBe` g a b

equivalentWhenFirstSucceedsOnValids2
    :: (Show a, Eq a, GenValidity a,
        Show b, Eq b, GenValidity b,
        Show c, Eq c,
        CanFail f)
    => (a -> b -> f c)
    -> (a -> b -> c)
    -> Property
equivalentWhenFirstSucceedsOnValids2 f g
    = equivalentWhenFirstSucceedsOnGens2 f g genValid

equivalentWhenFirstSucceeds2
    :: (Show a, Eq a, GenValidity a,
        Show b, Eq b, GenValidity b,
        Show c, Eq c,
        CanFail f)
    => (a -> b -> f c)
    -> (a -> b -> c)
    -> Property
equivalentWhenFirstSucceeds2 f g
    = equivalentWhenFirstSucceedsOnGens2 f g genUnchecked

equivalentWhenSecondSucceedsOnGen
    :: (Show a, Eq a, Show b, Eq b, CanFail f)
    => (a -> b)
    -> (a -> f b)
    -> Gen a
    -> Property
equivalentWhenSecondSucceedsOnGen f g gen =
    forAll gen $ \a ->
        case resultIfSucceeded (g a) of
            Nothing -> return () -- fine
            Just r  -> r `shouldBe` f a

equivalentWhenSecondSucceedsOnValid
    :: (Show a, Eq a, GenValidity a, Show b, Eq b, CanFail f)
    => (a -> b)
    -> (a -> f b)
    -> Property
equivalentWhenSecondSucceedsOnValid f g
    = equivalentWhenSecondSucceedsOnGen f g genValid

equivalentWhenSecondSucceeds
    :: (Show a, Eq a, GenValidity a, Show b, Eq b, CanFail f)
    => (a -> b)
    -> (a -> f b)
    -> Property
equivalentWhenSecondSucceeds f g
    = equivalentWhenSecondSucceedsOnGen f g genUnchecked

equivalentWhenSecondSucceedsOnGens2
    :: (Show a, Eq a,
        Show b, Eq b,
        Show c, Eq c,
        CanFail f)
    => (a -> b -> c)
    -> (a -> b -> f c)
    -> Gen (a, b)
    -> Property
equivalentWhenSecondSucceedsOnGens2 f g gen =
    forAll gen $ \(a, b) ->
        case resultIfSucceeded (g a b) of
            Nothing -> return () -- fine
            Just rs -> rs `shouldBe` f a b

equivalentWhenSecondSucceedsOnValids2
    :: (Show a, Eq a, GenValidity a,
         Show b, Eq b, GenValidity b,
         Show c, Eq c,
         CanFail f)
    => (a -> b -> c)
    -> (a -> b -> f c)
    -> Property
equivalentWhenSecondSucceedsOnValids2 f g
    = equivalentWhenSecondSucceedsOnGens2 f g genValid

equivalentWhenSecondSucceeds2
    :: (Show a, Eq a, GenValidity a,
        Show b, Eq b, GenValidity b,
        Show c, Eq c,
        CanFail f)
    => (a -> b -> c)
    -> (a -> b -> f c)
    -> Property
equivalentWhenSecondSucceeds2 f g
    = equivalentWhenSecondSucceedsOnGens2 f g genUnchecked

equivalentWhenSucceedOnGen
    :: (Show a, Eq a, Show b, Eq b, CanFail f)
    => (a -> f b)
    -> (a -> f b)
    -> Gen a
    -> Property
equivalentWhenSucceedOnGen f g gen =
    forAll gen $ \a ->
        case do fa <- resultIfSucceeded $ f a
                ga <- resultIfSucceeded $ g a
                return (fa, ga)
            of
            Nothing -> return () -- fine
            Just (fa, ga)  -> fa `shouldBe` ga

equivalentWhenSucceedOnValid
    :: (Show a, Eq a, GenValidity a, Show b, Eq b, CanFail f)
    => (a -> f b)
    -> (a -> f b)
    -> Property
equivalentWhenSucceedOnValid f g
    = equivalentWhenSucceedOnGen f g genValid

equivalentWhenSucceed
    :: (Show a, Eq a, GenValidity a, Show b, Eq b, CanFail f)
    => (a -> f b)
    -> (a -> f b)
    -> Property
equivalentWhenSucceed f g
    = equivalentWhenSucceedOnGen f g genUnchecked

equivalentWhenSucceedOnGens2
    :: (Show a, Eq a,
        Show b, Eq b,
        Show c, Eq c,
        CanFail f)
    => (a -> b -> f c)
    -> (a -> b -> f c)
    -> Gen (a, b)
    -> Property
equivalentWhenSucceedOnGens2 f g gen =
    forAll gen $ \(a, b) ->
        case do fab <- resultIfSucceeded $ f a b
                gab <- resultIfSucceeded $ g a b
                return (fab, gab)
                of
            Nothing -> return () -- fine
            Just (fab, gab) -> fab `shouldBe` gab

equivalentWhenSucceedOnValids2
    :: (Show a, Eq a, GenValidity a,
        Show b, Eq b, GenValidity b,
        Show c, Eq c,
        CanFail f)
    => (a -> b -> f c)
    -> (a -> b -> f c)
    -> Property
equivalentWhenSucceedOnValids2 f g
    = equivalentWhenSucceedOnGens2 f g genValid

equivalentWhenSucceed2
    :: (Show a, Eq a, GenValidity a,
        Show b, Eq b, GenValidity b,
        Show c, Eq c,
        CanFail f)
    => (a -> b -> f c)
    -> (a -> b -> f c)
    -> Property
equivalentWhenSucceed2 f g
    = equivalentWhenSucceedOnGens2 f g genUnchecked
