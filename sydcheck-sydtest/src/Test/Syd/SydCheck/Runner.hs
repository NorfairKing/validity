{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Syd.SydCheck.Runner (runSydCheckPropertyWithArgs) where

import Test.Syd
import Test.SydCheck
import Test.SydCheck.PList
import Test.SydCheck.Runner

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
runSydCheckPropertyWithArgs computeProperty TestRunSettings {..} progressReporter wrapper = do
  let report = reportProgress progressReporter
  -- TODO progress per example too
  -- TODO progress for when shrinking starts

  report ProgressTestStarting
  errOrResult <- applyWrapper2 wrapper $ \outers inner -> do
    let mSeed = case testRunSettingSeed of
          RandomSeed -> Nothing
          FixedSeed s -> Just (fromIntegral s)
    runTypedPropertyT
      testRunSettingMaxSuccess
      (Size testRunSettingMaxSize)
      (fromIntegral testRunSettingMaxShrinks)
      testRunSettingMaxDiscardRatio
      mSeed
      (computeProperty outers inner)
  report ProgressTestDone

  let (testRunResultStatus, testRunResultException, testRunResultNumTests, testRunResultNumShrinks, testRunResultFailingInputs, testRunResultExtraInfo) = case errOrResult of
        Left someException ->
          ( TestFailed,
            Just someException,
            Nothing,
            Nothing,
            [],
            Nothing
          )
        Right result -> case result of
          ResultGeneratorFailed errs ->
            ( TestFailed,
              Nothing,
              Just $ fromIntegral $ length errs,
              Nothing,
              [],
              Just $ unlines $ "Failed to generate enough values: " : errs
            )
          ResultNoCounterexample ->
            ( TestPassed,
              Nothing,
              Just $ fromIntegral testRunSettingMaxSuccess,
              Nothing,
              [],
              Nothing
            )
          ResultCounterexample counterexample examplesRun shrinksUsed exception ->
            -- TODO: number of tests when a counterexample is found.
            ( TestFailed,
              Just exception,
              Just examplesRun,
              Just shrinksUsed,
              showPList counterexample,
              Nothing
            )

  let testRunResultLabels = Nothing
  let testRunResultClasses = Nothing
  let testRunResultTables = Nothing
  let testRunResultGoldenCase = Nothing

  pure TestRunResult {..}

showPList :: PList ls -> [String]
showPList = \case
  PNil -> []
  PCons e pl -> ppShow e : showPList pl
