{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SydCheck where

import Control.Monad
import Data.Maybe
import SydCheck.Gen
import SydCheck.PList
import SydCheck.Property
import SydCheck.Shrinking
import System.Random.SplitMix as SM

-- | Run a generator. The size passed to the generator is always 30;
-- if you want another size then you should explicitly use 'resize'.
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

-- | Run a property test for any 'IsProperty'
--
-- Returns a counterexample if it succeeds, and
-- 'Left' with all the generation errors if no values could be generated within the given number of allowed discard attempts.
--
-- TODO use a record instead of this many parameters
runIsProperty ::
  forall ls prop.
  IsProperty ls prop =>
  Int ->
  Size ->
  Int ->
  Int ->
  Seed ->
  prop ->
  Either [String] (Maybe (PList ls)) -- Counterexample
runIsProperty successes maxSize maxShrinks maxDiscardRatio seed prop =
  runProperty successes maxSize maxShrinks maxDiscardRatio seed $
    toProperty prop

-- | Run a property test and shrink if it Fails'
--
-- Returns a counterexample if it succeeds, and
-- 'Left' with all the generation errors if no values could be generated within the given number of allowed discard attempts.
runProperty ::
  forall ls.
  Int ->
  Size ->
  Int ->
  Int ->
  Seed ->
  Property ls ->
  Either [String] (Maybe (PList ls)) -- Counterexample
runProperty successes maxSize maxShrinks maxDiscardRatio initialSeed prop =
  let sizes = computeSizes successes maxSize
   in -- TODO make sure the seeds are randomly generated instead of sequential
      go [] $ zip sizes [initialSeed ..]
  where
    go :: [String] -> [(Size, Seed)] -> Either [String] (Maybe (PList ls))
    go generationErrors = \case
      [] -> Right Nothing -- Could not find a counterexample
      ((size, seed) : rest) ->
        case runPropertyOn maxShrinks maxDiscardRatio size seed prop of
          Left errs -> Left (generationErrors ++ errs)
          Right (errs, (values, result)) ->
            if result
              then go (generationErrors ++ errs) rest
              else Right $ Just values -- Fonud a counterexample

computeSizes :: Int -> Size -> [Size]
computeSizes successes maxSize = case successes of
  0 -> []
  1 -> [0]
  2 -> [0, maxSize]
  n -> [0] ++ [i * maxSize `div` (n - 1) | i <- [1 .. n - 2]] ++ [maxSize]

-- | Evaluate a property once and shrink if it fails.
--
-- 'Left' with all the generation errors if no values could be generated within the given number of allowed discard attempts.
runPropertyOn ::
  Int ->
  Int ->
  Size ->
  Seed ->
  Property ls ->
  Either [String] ([String], (PList ls, Bool))
runPropertyOn maxShrinks maxDiscards size seed prop = do
  ((ws, errs), (values, result)) <- runPropertyOnRandomnessWithDiscards maxDiscards size seed prop
  pure
    ( errs,
      if result
        then (values, result)
        else case shrinkProperty maxShrinks ws prop of
          Nothing -> (values, result)
          Just values' -> (values', result)
    )

-- | Evaluate a property once with a maximum number of discarded generation attemtps.
--
-- 'Left' with all the generation errors if no values could be generated within the given number of allowed discard attempts.
runPropertyOnRandomnessWithDiscards ::
  Int ->
  Size ->
  Seed ->
  Property ls ->
  Either [String] ((Randomness, [String]), (PList ls, Bool))
runPropertyOnRandomnessWithDiscards maxDiscards initialSize initialSeed prop = go [] maxDiscards initialSize initialSeed
  where
    go errs discardsLeft size seed =
      if discardsLeft <= 0
        then Left errs
        else
          let ws = computeRandomness size seed
           in case runPropertyOnRandomness ws prop of
                Left err ->
                  go
                    (err : errs)
                    (pred discardsLeft)
                    (succ size) -- TODO maybe increase the size faster?
                    (succ seed) -- TODO maybe use a more random seed instead of just the next one?
                Right (values, result) -> Right ((ws, errs), (values, result))

-- | Evaluate a property once, 'Left' if the values couldn't be generated.
runPropertyOnRandomness ::
  Randomness ->
  Property ls ->
  Either String (PList ls, Bool)
runPropertyOnRandomness = go
  where
    go :: Randomness -> Property ls -> Either String (PList ls, Bool)
    go ws = \case
      PropBool b -> Right (PNil, b)
      PropGen gen func -> do
        let (usedRandomness, restRandomness) = computeSplitRandomness ws
        value <- runGen gen usedRandomness
        (generateds, result) <- go restRandomness (func value)
        pure (PCons value generateds, result)

-- | Shrink a property a given maximum number of times.
--
-- Return the shrunk inputs
shrinkProperty ::
  forall ls.
  Int ->
  Randomness ->
  Property ls ->
  Maybe (PList ls)
shrinkProperty maxShrinks r prop =
  case shrinkPropertyAndReturnAllShrinks maxShrinks r prop of
    [] -> Nothing
    ls -> Just $ last ls

-- | Shrink a property a given maximum number of times
--
-- Return all shrunk versions of the inputs.
--
-- TODO record how many shrinks were done
shrinkPropertyAndReturnAllShrinks ::
  forall ls.
  Int ->
  Randomness ->
  Property ls ->
  [PList ls]
shrinkPropertyAndReturnAllShrinks maxShrinks r prop = go maxShrinks r
  where
    go :: Int -> Randomness -> [PList ls]
    go currentShrinksLeft ws =
      let shrinks = shrinkPropertyOneStep currentShrinksLeft ws prop
       in case listToMaybe shrinks of
            Nothing -> []
            Just (triesDone, (ws', values)) ->
              let newShrinksLeft = max 0 (currentShrinksLeft - triesDone)
               in values : go newShrinksLeft ws'

shrinkPropertyOneStep ::
  Int ->
  Randomness ->
  Property ls ->
  [(Int, (Randomness, PList ls))]
shrinkPropertyOneStep maxShrinksThisRound ws prop =
  let shrunkRandomnesses = take maxShrinksThisRound (shrinkRandomness ws)
   in do
        (triesDone, ws') <- zip [1 ..] shrunkRandomnesses
        case runPropertyOnRandomness ws' prop of
          Left _ -> []
          Right (vals, result) -> do
            guard $ not result
            pure (triesDone, (ws', vals))
