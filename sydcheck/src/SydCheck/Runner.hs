{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SydCheck.Runner where

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
  forall m ls prop.
  Monad m =>
  IsTypedPropertyT ls m prop =>
  Int ->
  Size ->
  Int ->
  Int ->
  Seed ->
  prop ->
  m (Either [String] (Maybe (PList ls))) -- Counterexample
runIsTypedPropertyT successes maxSize maxShrinks maxDiscardRatio seed prop =
  runTypedPropertyT successes maxSize maxShrinks maxDiscardRatio seed $
    toTypedPropertyT prop

-- | Run a property test and shrink if it Fails'
--
-- Returns a counterexample if it succeeds, and
-- 'Left' with all the generation errors if no values could be generated within the given number of allowed discard attempts.
runTypedPropertyT ::
  forall m ls.
  Monad m =>
  Int ->
  Size ->
  Int ->
  Int ->
  Seed ->
  TypedPropertyT ls m ->
  m (Either [String] (Maybe (PList ls))) -- Counterexample
runTypedPropertyT successes maxSize maxShrinks maxDiscardRatio initialSeed prop =
  let sizes = computeSizes successes maxSize
   in -- TODO make sure the seeds are randomly generated instead of sequential
      go [] $ zip sizes [initialSeed ..]
  where
    go :: [String] -> [(Size, Seed)] -> m (Either [String] (Maybe (PList ls)))
    go generationErrors = \case
      [] -> pure (Right Nothing) -- Could not find a counterexample
      ((size, seed) : rest) -> do
        trip <- runPropertyOn maxShrinks maxDiscardRatio size seed prop
        case trip of
          Left errs -> pure $ Left (generationErrors ++ errs)
          Right (errs, (values, result)) ->
            if result
              then go (generationErrors ++ errs) rest
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
  Monad m =>
  Int ->
  Int ->
  Size ->
  Seed ->
  TypedPropertyT ls m ->
  m (Either [String] ([String], (PList ls, Bool)))
runPropertyOn maxShrinks maxDiscards size seed prop = do
  errOrTup <- runPropertyOnRandomnessWithDiscards maxDiscards size seed prop
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
  forall m ls.
  Monad m =>
  Int ->
  Size ->
  Seed ->
  TypedPropertyT ls m ->
  m (Either [String] ((Randomness, [String]), (PList ls, Bool)))
runPropertyOnRandomnessWithDiscards maxDiscards initialSize initialSeed prop = go [] maxDiscards initialSize initialSeed
  where
    go ::
      [String] ->
      Int ->
      Size ->
      Seed ->
      m (Either [String] ((Randomness, [String]), (PList ls, Bool)))
    go errs discardsLeft size seed =
      if discardsLeft <= 0
        then pure $ Left errs
        else do
          let ws = computeRandomness size seed
          errOrResult <- runPropertyOnRandomness ws prop
          case errOrResult of
            Left err ->
              go
                (err : errs)
                (pred discardsLeft)
                (succ size) -- TODO maybe increase the size faster?
                (succ seed) -- TODO maybe use a more random seed instead of just the next one?
            Right (values, result) ->
              pure $ Right ((ws, errs), (values, result))

-- | Evaluate a property once, 'Left' if the values couldn't be generated.
runPropertyOnRandomness ::
  forall m ls.
  Monad m =>
  Randomness ->
  TypedPropertyT ls m ->
  m (Either String (PList ls, Bool))
runPropertyOnRandomness = go
  where
    go ::
      forall m ls.
      Monad m =>
      Randomness ->
      TypedPropertyT ls m ->
      m (Either String (PList ls, Bool))
    go ws = \case
      PropBool b -> pure $ Right (PNil, b)
      PropGen gen func -> do
        let (usedRandomness, restRandomness) = computeSplitRandomness ws
        case runGen gen usedRandomness of
          Left err -> pure $ Left err
          Right value -> do
            errOrTup <- go restRandomness (func value)
            pure $ do
              (generateds, result) <- errOrTup
              pure (PCons value generateds, result)

-- | Shrink a property a given maximum number of times.
--
-- Return the shrunk inputs
shrinkProperty ::
  forall m ls.
  Monad m =>
  Int ->
  Randomness ->
  TypedPropertyT ls m ->
  m (Maybe (PList ls))
shrinkProperty maxShrinks r prop = do
  shrinks <- shrinkPropertyAndReturnAllShrinks maxShrinks r prop
  pure $ case shrinks of
    [] -> Nothing
    ls -> Just $ last ls

-- | Shrink a property a given maximum number of times
--
-- Return all shrunk versions of the inputs.
--
-- TODO record how many shrinks were done
shrinkPropertyAndReturnAllShrinks ::
  forall m ls.
  Monad m =>
  Int ->
  Randomness ->
  TypedPropertyT ls m ->
  m [PList ls]
shrinkPropertyAndReturnAllShrinks maxShrinks r prop = go maxShrinks r
  where
    go :: Int -> Randomness -> m [PList ls]
    go currentShrinksLeft ws = do
      mShrink <- shrinkPropertyOneStep currentShrinksLeft ws prop
      case mShrink of
        Nothing -> pure []
        Just (triesDone, (ws', values)) -> do
          let newShrinksLeft = max 0 (currentShrinksLeft - triesDone)
          (values :) <$> go newShrinksLeft ws'

shrinkPropertyOneStep ::
  forall m ls.
  Monad m =>
  Int ->
  Randomness ->
  TypedPropertyT ls m ->
  m (Maybe (Int, (Randomness, PList ls)))
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
