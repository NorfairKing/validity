{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Syd.SydCheck.Runner (runSydCheckPropertyWithArgs) where

import Data.Typeable
import SydCheck
import Test.Syd
import Test.Syd.SydCheck.Utils

instance IsTest (TypedPropertyT ls IO) where
  type Arg1 (TypedPropertyT ls IO) = ()
  type Arg2 (TypedPropertyT ls IO) = ()
  runTest func = runTest (\() () -> func)

instance IsTest (arg -> TypedPropertyT ls IO) where
  type Arg1 (arg -> TypedPropertyT ls IO) = ()
  type Arg2 (arg -> TypedPropertyT ls IO) = arg
  runTest func = runTest (\() arg -> func arg)

instance IsTest (outerArgs -> innerArg -> TypedPropertyT ls IO) where
  type Arg1 (outerArgs -> innerArg -> TypedPropertyT ls IO) = outerArgs
  type Arg2 (outerArgs -> innerArg -> TypedPropertyT ls IO) = innerArg
  runTest = runSydCheckPropertyWithArgs

runSydCheckPropertyWithArgs ::
  (outerArgs -> innerArg -> TypedProperty ls) ->
  TestRunSettings ->
  ProgressReporter ->
  ((outerArgs -> innerArg -> IO ()) -> IO ()) ->
  IO TestRunResult
runSydCheckPropertyWithArgs computeProperty = undefined
