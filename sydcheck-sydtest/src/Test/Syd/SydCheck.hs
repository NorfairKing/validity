{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.SydCheck
  ( -- * Typeclass specifications
    eqSpec,
    ordSpec,
    showReadSpec,

    -- * Function specifications
    shouldBeValid,
    producesValid,
    producesValid2,

    -- ** Utils
    nameOf,

    -- ** Reexport SydCheck
    module SydCheck,
  )
where

import Data.Typeable
import SydCheck
import Test.Syd

eqSpec :: forall a. Spec
eqSpec = pure ()

ordSpec :: forall a. Spec
ordSpec = pure ()

showReadSpec :: forall a. Spec
showReadSpec = pure ()

shouldBeValid :: a -> IO ()
shouldBeValid = undefined

producesValid :: (a -> b) -> TypedProperty '[a]
producesValid = undefined

producesValid2 :: (a -> b -> c) -> TypedProperty '[a, b]
producesValid2 = undefined

nameOf ::
  forall a.
  Typeable a =>
  String
nameOf = showsPrec 10 (typeRep (Proxy @a)) ""

instance IsTest (TypedProperty ls) where
  type Arg1 (TypedProperty ls) = ()
  type Arg2 (TypedProperty ls) = ()
  runTest func = runTest (\() () -> func)

instance IsTest (arg -> TypedProperty ls) where
  type Arg1 (arg -> TypedProperty ls) = ()
  type Arg2 (arg -> TypedProperty ls) = arg
  runTest func = runTest (\() arg -> func arg)

instance IsTest (outerArgs -> innerArg -> TypedProperty ls) where
  type Arg1 (outerArgs -> innerArg -> TypedProperty ls) = outerArgs
  type Arg2 (outerArgs -> innerArg -> TypedProperty ls) = innerArg
  runTest = runSydCheckPropertyWithArgs

runSydCheckPropertyWithArgs ::
  (outerArgs -> innerArg -> TypedProperty ls) ->
  TestRunSettings ->
  ProgressReporter ->
  ((outerArgs -> innerArg -> IO ()) -> IO ()) ->
  IO TestRunResult
runSydCheckPropertyWithArgs computeProperty = undefined
