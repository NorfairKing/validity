{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SydCheck.Runner where

import Control.Exception
import Control.Monad
import Data.Maybe
import SydCheck.Gen
import SydCheck.PList
import SydCheck.Property
import SydCheck.Randomness
import SydCheck.Shrinking
import System.Random.SplitMix as SM

-- | Run a generator. The size passed to the generator is always 30;
-- if you want another size then you should explicitly use 'generateOfSize'.
generate :: Gen a -> IO (Either String a)
generate = generateOfSize 30

generateOfSize :: Size -> Gen a -> IO (Either String a)
generateOfSize size g = do
  smGen <- initSMGen
  let randomness = computeRandomnessWithSMGen size smGen
  pure $ runGen g randomness

-- | Generates some example values.
sample' :: Gen a -> IO [Either String a]
sample' g = mapM (\size -> generateOfSize size g) [0, 2 .. 20]

-- | Generates some example values and prints them to 'stdout'.
sample :: Show a => Gen a -> IO ()
sample g = do
  samples <- sample' g
  mapM_ print samples

-- | Run a property test for any 'IsTypedTypedPropertyT
--
-- Returns a counterexample if it succeeds, and
-- 'Left' with all the generation errors if no values could be generated within the given number of allowed discard attempts.
--
-- TODO use a record instead of this many parameters
runIsTypedPropertyT ::
  forall ls prop.
  IsTypedPropertyT ls IO prop =>
  Int ->
  Size ->
  Int ->
  Int ->
  Maybe Seed ->
  prop ->
  IO (Either [String] (Maybe (PList ls))) -- Counterexample
runIsTypedPropertyT successes maxSize maxShrinks maxDiscardRatio seed prop =
  runTypedPropertyT successes maxSize maxShrinks maxDiscardRatio seed $
    toTypedPropertyT prop

-- | Run a property test and shrink if it Fails'
--
-- Returns a counterexample if it succeeds, and
-- 'Left' with all the generation errors if no values could be generated within the given number of allowed discard attempts.
runTypedPropertyT ::
  forall ls.
  Int ->
  Size ->
  Int ->
  Int ->
  Maybe Seed ->
  TypedPropertyT ls IO ->
  IO (Either [String] (Maybe (PList ls))) -- Counterexample
runTypedPropertyT successes maxSize maxShrinks maxDiscardRatio mSeed prop = do
  initialGen <- case mSeed of
    Just seed -> pure $ mkSMGen seed
    Nothing -> newSMGen
  let sizes = computeSizes successes maxSize
  go initialGen [] sizes
  where
    go :: SMGen -> [String] -> [Size] -> IO (Either [String] (Maybe (PList ls)))
    go gen generationErrors = \case
      [] -> pure (Right Nothing) -- Could not find a counterexample
      (size : rest) -> do
        let (thisGen, nextGen) = splitSMGen gen
        trip <- runPropertyOn maxShrinks maxDiscardRatio size thisGen prop
        case trip of
          Left errs -> pure $ Left (generationErrors ++ errs)
          Right (errs, (values, result)) ->
            if result
              then go nextGen (generationErrors ++ errs) rest
              else pure $ Right $ Just values -- Found a counterexample

computeSizes :: Int -> Size -> [Size]
computeSizes successes (Size maxSize) = case successes of
  0 -> []
  1 -> [0]
  2 -> [0, Size maxSize]
  n -> [0] ++ [Size $ i * maxSize `div` (n - 1) | i <- [1 .. n - 2]] ++ [Size maxSize]

-- | Evaluate a property once and shrink if it fails.
--
-- 'Left' with all the generation errors if no values could be generated within the given number of allowed discard attempts.
runPropertyOn ::
  Int ->
  Int ->
  Size ->
  SMGen ->
  TypedPropertyT ls IO ->
  IO (Either [String] ([String], (PList ls, Bool)))
runPropertyOn maxShrinks maxDiscards size gen prop = do
  errOrTup <- runPropertyOnRandomnessWithDiscards maxDiscards size gen prop
  case errOrTup of
    Left err -> pure $ Left err
    Right ((ws, errs), (values, result)) -> do
      rightTup <-
        if result
          then pure (values, result)
          else do
            mShrunk <- shrinkProperty maxShrinks ws prop
            pure $ case mShrunk of
              Nothing -> (values, result)
              Just values' -> (values', result)
      pure $ Right (errs, rightTup)

-- | Evaluate a property once with a maximum number of discarded generation attemtps.
--
-- 'Left' with all the generation errors if no values could be generated within the given number of allowed discard attempts.
runPropertyOnRandomnessWithDiscards ::
  forall ls.
  Int ->
  Size ->
  SMGen ->
  TypedPropertyT ls IO ->
  IO (Either [String] ((Randomness, [String]), (PList ls, Bool)))
runPropertyOnRandomnessWithDiscards maxDiscards initialSize gen prop =
  go [] maxDiscards initialSize gen
  where
    go ::
      [String] ->
      Int ->
      Size ->
      SMGen ->
      IO (Either [String] ((Randomness, [String]), (PList ls, Bool)))
    go errs discardsLeft size gen =
      if discardsLeft <= 0
        then pure $ Left errs
        else do
          let (thisGen, nextGen) = splitSMGen gen
          let ws = computeRandomnessWithSMGen size thisGen
          errOrResult <- runPropertyOnRandomness ws prop
          case errOrResult of
            Left err ->
              go
                (err : errs)
                (pred discardsLeft)
                (succ size) -- TODO maybe increase the size faster?
                nextGen
            Right (values, result) ->
              pure $ Right ((ws, errs), (values, result))

-- | Evaluate a property once, 'Left' if the values couldn't be generated.
runPropertyOnRandomness ::
  forall ls.
  Randomness ->
  TypedPropertyT ls IO ->
  IO (Either String (PList ls, Bool))
runPropertyOnRandomness = go
  where
    go ::
      forall ls.
      Randomness ->
      TypedPropertyT ls IO ->
      IO (Either String (PList ls, Bool))
    go ws = \case
      PropAction m -> do
        errOrUnit <- (Right <$> m) `catches` exceptionHandlers
        pure $
          Right
            ( PNil,
              case errOrUnit of
                Left _ -> False
                Right () -> True
            )
      PropGen gen func -> do
        let (usedRandomness, restRandomness) = computeSplitRandomness ws
        case runGen gen usedRandomness of
          Left err -> pure $ Left err
          Right value -> do
            errOrTup <- go restRandomness (func value)
            pure $ do
              (generateds, result) <- errOrTup
              pure (PCons value generateds, result)

exceptionHandlers :: [Handler (Either SomeException a)]
exceptionHandlers =
  [ -- Re-throw AsyncException, otherwise execution will not terminate on SIGINT (ctrl-c).
    Handler (\e -> throwIO (e :: AsyncException)),
    -- Catch all the rest
    Handler (\e -> return $ Left (e :: SomeException))
  ]

-- | Shrink a property a given maximum number of times.
--
-- Return the shrunk inputs
shrinkProperty ::
  forall ls.
  Int ->
  Randomness ->
  TypedPropertyT ls IO ->
  IO (Maybe (PList ls))
shrinkProperty maxShrinks r prop = do
  shrinks <- shrinkPropertyAndReturnAllShrinks maxShrinks r prop
  pure $ case shrinks of
    [] -> Nothing
    ls -> Just $ last ls

-- | Shrink a property a given maximum number of times
--
-- Return all shrunk versions of the inputs.
--
-- TODO do we need this to be separate from 'shrinkProperty' at all?
-- TODO report how many shrinks were done.
shrinkPropertyAndReturnAllShrinks ::
  forall ls.
  Int ->
  Randomness ->
  TypedPropertyT ls IO ->
  IO [PList ls]
shrinkPropertyAndReturnAllShrinks maxShrinks r prop = go maxShrinks r
  where
    go :: Int -> Randomness -> IO [PList ls]
    go currentShrinksLeft ws = do
      mShrink <- shrinkPropertyOneStep currentShrinksLeft ws prop
      case mShrink of
        Nothing -> pure []
        Just (triesDone, (ws', values)) -> do
          let newShrinksLeft = max 0 (currentShrinksLeft - triesDone)
          (values :) <$> go newShrinksLeft ws'

shrinkPropertyOneStep ::
  forall ls.
  Int ->
  Randomness ->
  TypedPropertyT ls IO ->
  IO (Maybe (Int, (Randomness, PList ls)))
shrinkPropertyOneStep maxShrinksThisRound ws prop =
  go $ zip [1 ..] (take maxShrinksThisRound (shrinkRandomness ws))
  where
    go [] = pure Nothing
    go ((triesDone, shrunkRandomness) : rest) = do
      errOrTup <- runPropertyOnRandomness shrunkRandomness prop
      case errOrTup of
        Left _ -> go rest
        Right (values, result) ->
          if result
            then go rest
            else pure $ Just (triesDone, (shrunkRandomness, values))
