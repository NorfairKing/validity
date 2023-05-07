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
import Data.Set (Set)
import qualified Data.Set as S
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

data Result ls
  = ResultNoCounterexample
  | ResultGeneratorFailed ![String]
  | ResultCounterexample
      !(PList ls)
      -- ^ Counterexample
      !Word
      -- ^ Tests run
      !Word
      -- ^ Shrinks used
      !SomeException
      -- ^ Exception that caused the test to fail
  deriving (Show)

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
  Word ->
  Int ->
  Maybe Seed ->
  prop ->
  IO (Result ls)
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
  Word ->
  Int ->
  Maybe Seed ->
  TypedProperty ls ->
  IO (Result ls)
runTypedPropertyT successes maxSize maxShrinks maxDiscardRatio mSeed prop = do
  initialGen <- case mSeed of
    Just seed -> pure $ mkSMGen seed
    Nothing -> newSMGen
  let sizes = computeSizes successes maxSize
  go initialGen [] sizes
  where
    go :: SMGen -> [String] -> [Size] -> IO (Result ls)
    go gen generationErrors = \case
      [] -> pure ResultNoCounterexample -- Could not find a counterexample
      (size : rest) -> do
        let (thisGen, nextGen) = splitSMGen gen
        trip <- runPropertyOn maxShrinks maxDiscardRatio size thisGen prop
        case trip of
          Left errs -> pure $ ResultGeneratorFailed (generationErrors ++ errs)
          Right (errs, (values, mShrinksAndException)) ->
            case mShrinksAndException of
              Nothing -> go nextGen (generationErrors ++ errs) rest
              Just (shrinksUsed, exception) ->
                -- Found a counterexample
                pure $
                  ResultCounterexample
                    values
                    (fromIntegral (successes - length rest))
                    shrinksUsed
                    exception

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
  Word ->
  Int ->
  Size ->
  SMGen ->
  TypedProperty ls ->
  IO (Either [String] ([String], (PList ls, Maybe (Word, SomeException))))
runPropertyOn maxShrinks maxDiscards size gen prop = do
  errOrTup <- runPropertyOnRandomnessWithDiscards maxDiscards size gen prop
  case errOrTup of
    Left err -> pure $ Left err
    Right ((ws, errs), (values, result)) -> do
      rightTup <- case result of
        Nothing -> pure (values, Nothing)
        Just exception -> do
          mShrunk <- shrinkProperty maxShrinks ws prop
          pure $ case mShrunk of
            Nothing -> (values, Just (0, exception))
            Just (shrinkIx, (values', exception')) -> (values', Just (shrinkIx, exception'))
      pure $ Right (errs, rightTup)

-- | Evaluate a property once with a maximum number of discarded generation attemtps.
--
-- 'Left' with all the generation errors if no values could be generated within the given number of allowed discard attempts.
runPropertyOnRandomnessWithDiscards ::
  forall ls.
  Int ->
  Size ->
  SMGen ->
  TypedProperty ls ->
  IO (Either [String] ((Randomness, [String]), (PList ls, Maybe SomeException)))
runPropertyOnRandomnessWithDiscards maxDiscards initialSize gen prop =
  go [] maxDiscards initialSize gen
  where
    go ::
      [String] ->
      Int ->
      Size ->
      SMGen ->
      IO (Either [String] ((Randomness, [String]), (PList ls, Maybe SomeException)))
    go errs discardsLeft size g =
      if discardsLeft <= 0
        then pure $ Left errs
        else do
          let (thisGen, nextGen) = splitSMGen g
          let ws = computeRandomnessWithSMGen size thisGen
          errOrResult <- runPropertyOnRandomness ws prop
          case errOrResult of
            Left err ->
              go
                (err : errs)
                (pred discardsLeft)
                (succ size) -- TODO maybe increase the size faster?
                nextGen
            Right (values, result) -> do
              pure $ Right ((ws, errs), (values, result))

-- | Evaluate a property once, 'Left' if the values couldn't be generated.
runPropertyOnRandomness ::
  forall ls.
  Randomness ->
  TypedProperty ls ->
  IO (Either String (PList ls, Maybe SomeException))
runPropertyOnRandomness = go
  where
    go ::
      forall ls'.
      Randomness ->
      TypedProperty ls' ->
      IO (Either String (PList ls', Maybe SomeException))
    go ws = \case
      PropAction m -> do
        errOrUnit <- (Right <$> m) `catches` exceptionHandlers
        pure $
          Right
            ( PNil,
              case errOrUnit of
                Left exception -> Just exception
                Right () -> Nothing
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
  Word ->
  Randomness ->
  TypedProperty ls ->
  IO (Maybe (Word, (PList ls, SomeException)))
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
shrinkPropertyAndReturnAllShrinks ::
  forall ls.
  Word ->
  Randomness ->
  TypedProperty ls ->
  IO [(Word, (PList ls, SomeException))]
shrinkPropertyAndReturnAllShrinks maxShrinks r prop = go S.empty maxShrinks r
  where
    go :: Set Randomness -> Word -> Randomness -> IO [(Word, (PList ls, SomeException))]
    go attempts currentShrinksLeft ws = do
      (newAttempts, mShrink) <- shrinkPropertyOneStep attempts currentShrinksLeft ws prop
      case mShrink of
        Nothing -> pure []
        Just (triesDone, (ws', values)) -> do
          -- TODO: is this correct when it's a word?
          let newShrinksLeft = currentShrinksLeft - triesDone
          ((maxShrinks - newShrinksLeft, values) :) <$> go newAttempts newShrinksLeft ws'

shrinkPropertyOneStep ::
  forall ls.
  Set Randomness ->
  Word ->
  Randomness ->
  TypedProperty ls ->
  IO (Set Randomness, Maybe (Word, (Randomness, (PList ls, SomeException))))
shrinkPropertyOneStep initialAttempts maxShrinksThisRound ws prop =
  go initialAttempts 0 $ take (fromIntegral maxShrinksThisRound) (shrinkRandomness ws)
  where
    go attempts triesDoneThisRound = \case
      [] -> pure (attempts, Nothing)
      (shrunkRandomness : rest) -> do
        if S.member shrunkRandomness attempts
          then go attempts triesDoneThisRound rest
          else do
            errOrTup <- runPropertyOnRandomness shrunkRandomness prop
            let newAttempts = S.insert shrunkRandomness attempts
            let newTriesDone = succ triesDoneThisRound
            case errOrTup of
              Left _ -> go newAttempts newTriesDone rest
              Right (values, result) -> do
                case result of
                  Nothing -> go newAttempts newTriesDone rest
                  Just exception -> do
                    pure (newAttempts, Just (newTriesDone, (shrunkRandomness, (values, exception))))
