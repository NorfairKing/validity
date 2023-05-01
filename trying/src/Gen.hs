{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Gen where

import Control.Applicative
import Control.Monad
import Control.Selective
import Data.Either (rights)
import Data.Kind
import Data.Maybe
import Data.Tuple (swap)
import Data.Validity
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import Data.Word
import GHC.Float (castWord64ToDouble)
import System.Random.SplitMix as SM

-- TODO: Newtype?
type Size = Int

-- TODO: Newtype?
type Seed = RandomWord

-- TODO: Newtype?
type RandomWord = Word64

-- TODO: Newtype?
type Randomness = Vector Word64

-- Integrated shrinking AND size handling.
--
-- The length of the vector is the size paremeter, in some sense.
-- That's how much randomness the generator is allowed to use.
data Gen a where
  -- | Generator that uses a fixed amount of randomness
  GenFixedSize :: Size -> (Randomness -> Either String a) -> Gen a
  -- | Generator that uses a variable amount of randomness.
  GenVariableSize :: (Randomness -> Either String a) -> Gen a
  -- | Generator that uses the amount of randomness left over to decide what to do.
  GenSized :: (Size -> Gen a) -> Gen a
  -- | For the Functor instance
  GenPure :: a -> Gen a
  -- | For the Applicative instance
  GenFMap :: (a -> b) -> Gen a -> Gen b
  GenAp :: Gen (a -> b) -> Gen a -> Gen b
  -- | For the Alternative instance
  GenAlt :: Gen a -> Gen b -> Gen (Either a b)
  -- | For the Selective instance
  GenSelect :: Gen (Either a b) -> Gen (a -> b) -> Gen b
  -- | For the Monad instance
  GenBind :: Gen a -> (a -> Gen b) -> Gen b
  -- | For MonadFail
  GenFail :: String -> Gen a

instance Functor Gen where
  fmap = GenFMap

instance Applicative Gen where
  pure = GenPure
  (<*>) = GenAp

instance Alternative Gen where
  empty = fail ""
  (<|>) g1 g2 = either id id <$> GenAlt g1 g2

instance Selective Gen where
  select = GenSelect

instance Monad Gen where
  (>>=) = GenBind

instance MonadFail Gen where
  fail = GenFail

sized :: (Size -> Gen a) -> Gen a
sized = GenSized

-- resize is not available
-- scale is not available

sizeOfGen :: Gen a -> Maybe Size
sizeOfGen = go
  where
    -- Nothing here means both "we don't know" and "Not fixed size".
    go :: Gen a -> Maybe Size
    go = \case
      GenFixedSize w _ -> Just w
      GenVariableSize _ -> Nothing
      GenSized _ -> Nothing
      GenPure _ -> Just 0
      GenFMap _ g -> go g
      GenAp g1 g2 -> (+) <$> go g1 <*> go g2
      GenAlt g1 g2 -> max <$> go g1 <*> go g2
      GenSelect g1 g2 ->
        -- This may not be the actual size, but is definitely an upper bound
        (+) <$> go g1 <*> go g2
      GenBind _ _ -> Nothing
      GenFail _ -> Just 0

runGen :: Gen a -> Randomness -> Either String a
runGen = flip go
  where
    go :: Randomness -> Gen a -> Either String a
    go ws = \case
      GenFixedSize size fun -> fun (UV.take size ws)
      GenVariableSize fun -> fun ws
      GenSized fun -> go ws (fun (UV.length ws))
      GenPure a -> pure a
      GenFMap f g' -> f <$> go ws g'
      GenAp gf ga ->
        -- TODO the way this is called is O(n^2). That can probably be done better.
        let (leftWs, rightWs) = case (sizeOfGen gf, sizeOfGen ga) of
              (Nothing, Nothing) -> computeSplitRandomness ws
              (Just fsize, _) -> splitRandomnessAt fsize ws
              (_, Just asize) -> swap $ splitRandomnessAt asize ws
         in go leftWs gf <*> go rightWs ga
      GenAlt g1 g2 -> (Left <$> go ws g1) <|> (Right <$> go ws g2)
      GenSelect gEither gFun -> do
        let (leftWs, rightWs) = case (sizeOfGen gEither, sizeOfGen gFun) of
              (Nothing, Nothing) -> computeSplitRandomness ws
              (Just fsize, _) -> splitRandomnessAt fsize ws
              (_, Just asize) -> swap $ splitRandomnessAt asize ws
        e <- go leftWs gEither
        case e of
          Left a -> ($ a) <$> go rightWs gFun
          Right b -> pure b
      GenBind ga mb -> do
        let (leftWs, rightWs) =
              case sizeOfGen ga of
                Nothing -> computeSplitRandomness ws
                Just asize -> splitRandomnessAt asize ws
        a <- go leftWs ga
        go rightWs (mb a)
      GenFail err -> Left err -- TODO let runGen fail

runGenUntilSucceeds :: Size -> Seed -> Gen a -> (Randomness, a)
runGenUntilSucceeds initialSize initialSeed gen = go initialSize initialSeed
  where
    go size seed =
      let ws = computeRandomness size seed
       in case runGen gen ws of
            Left _ -> go (succ size) (succ seed)
            Right a -> (ws, a)

-- | Compute an arbitrarily split value
--
-- Input: number n
-- Output: number between 0 and n (inclusive), distributed uniformly
--
-- When randomWord is 0, the split is shrunk as much as possible.
-- In that case we want to return the most shrunk split, so (0, n)
computeSplit :: Int -> RandomWord -> Int
computeSplit totalSize randomWord =
  let left = randomWord `rem` (fromIntegral (totalSize + 1))
   in fromIntegral left

splitRandomnessAt :: Size -> Randomness -> (Randomness, Randomness)
splitRandomnessAt = UV.splitAt

computeSplitRandomness :: Randomness -> (Randomness, Randomness)
computeSplitRandomness ws =
  let len = UV.length ws
   in case len of
        0 -> (UV.empty, UV.empty)
        1 -> (UV.empty, UV.empty)
        _ ->
          let leftSize = computeSplit (pred len) (UV.head ws)
              restRandomness = UV.tail ws
           in UV.splitAt leftSize restRandomness

genFromSingleRandomWord :: (Maybe RandomWord -> a) -> Gen a
genFromSingleRandomWord func = GenFixedSize 1 $ \v ->
  pure $
    func $
      if UV.null v
        then Nothing
        else Just $ UV.head v

takeNextRandomWord :: Gen RandomWord
takeNextRandomWord = genFromSingleRandomWord $ \case
  Nothing -> 0
  Just w -> w

-- Laws:
-- 1: Every generated value must be valid
-- 2: With enough randomness, every valid value must be generated eventually.
--
-- Ideally the generated values are particularly anoying.
-- So we try to generate values around the bounds with increased probability
class Validity a => GenValid a where
  genValid :: Gen a

instance GenValid Bool where
  genValid = genBool False

genBool :: Bool -> Gen Bool
genBool minimal = genFromSingleRandomWord $ \case
  Nothing -> minimal
  Just w -> even w

instance (GenValid a, GenValid b) => GenValid (a, b) where
  genValid = (,) <$> genValid <*> genValid

instance GenValid a => GenValid (Maybe a) where
  genValid = genMaybeOf genValid

instance GenValid a => GenValid [a] where
  genValid = genListOf genValid

instance GenValid Word8 where
  genValid = genFromSingleRandomWord $ \case
    Nothing -> 0
    Just w -> fromIntegral (w `rem` (fromIntegral (maxBound :: Word8)))

instance GenValid Word64 where
  genValid = takeNextRandomWord

instance GenValid Double where
  genValid = castWord64ToDouble <$> takeNextRandomWord

-- | Run one of the following elements with corresponding frequency and shrink
-- to the first.
frequency :: [(Int, Gen a)] -> Gen a
frequency ls = do
  let total = sum $ map fst ls
  ix <- genInt (1, total)
  pick ix ls
  where
    pick :: Int -> [(Int, Gen a)] -> Gen a
    pick n ((k, x) : xs)
      | n <= k = x
      | otherwise = pick (n - k) xs
    pick _ [] = fail "TODO LOCATION.frequency: pick used with empty list"

-- | Run one of the following generators and shrink to the first.
oneof :: [Gen a] -> Gen a
oneof ls = do
  ix <- genInt (0, length ls - 1)
  ls !! ix

-- | Generate one of the following elements and shrink to the first.
elements :: [a] -> Gen a
elements ls =
  (ls !!) <$> genInt (0, length ls - 1)

-- | Generates a value that satisfies a predicate.
suchThat :: Gen a -> (a -> Bool) -> Gen a
suchThat gen predicate = do
  a <- gen
  if predicate a
    then pure a
    else fail "suchThat: predicate rejected the value."

-- | Generates a value for which the given function returns a 'Just', and then
-- applies the function.
suchThatMap :: Gen a -> (a -> Maybe b) -> Gen b
suchThatMap gen fun = do
  a <- gen
  case fun a of
    Nothing -> fail "suchThatMap: Failed to map the value."
    Just b -> pure b

-- | Tries to generate a value that satisfies a predicate.
--
-- Return 'Nothing' if that fails.
suchThatMaybe :: Gen a -> (a -> Bool) -> Gen (Maybe a)
suchThatMaybe gen predicate = do
  a <- gen
  pure $ do
    guard $ predicate a
    pure a

-- | Generate a 'Double' within a range, shrink to the lower end
genInt :: (Int, Int) -> Gen Int
genInt (lo, hi) = computeInt (lo, hi) <$> takeNextRandomWord

computeInt :: (Int, Int) -> RandomWord -> Int
computeInt (lo, hi) rw =
  -- TODO this probably fails for very large integers because of double precision (?)
  round $ computeDouble (fromIntegral lo, fromIntegral hi) rw

-- | Generate a 'Double' within a range, shrink to the lower end
genDouble :: (Double, Double) -> Gen Double
genDouble (lo, hi) = do
  computeDouble (lo, hi) <$> takeNextRandomWord

computeDouble :: (Double, Double) -> RandomWord -> Double
computeDouble (lo, hi) rw =
  -- 0: lo
  -- maxBound: hi
  -- n: (hi - lo) / maxBound * w
  lo + ((hi - lo) / fromIntegral (maxBound :: Word64)) * fromIntegral rw

-- | Generate a 'Double' uniformly within '[0,1]' and shrink to 0.
genProperFraction :: Gen Double
genProperFraction = computeProperFraction <$> takeNextRandomWord

computeProperFraction :: RandomWord -> Double
computeProperFraction = computeDouble (0, 1)

-- | Generate a 'Maybe' and shrink to Nothing
genMaybeOf :: Gen a -> Gen (Maybe a)
genMaybeOf gen = frequency [(1, pure Nothing), (3, Just <$> gen)]

-- Better splitting with fixed size lists
genListOf :: forall a. Gen a -> Gen [a]
genListOf gen = case sizeOfGen gen of
  Just asize ->
    GenVariableSize $ \ws -> case UV.length ws of
      0 -> pure []
      1 -> pure $ rights [runGen gen ws]
      _ -> do
        let s = UV.length ws
            maxLen = (s - 1) `div` asize
            len = computeIntFromTriangleDistribution 0 maxLen maxLen (ws UV.! 0)
        pure $ rights $ go (UV.drop 1 ws) (replicate len asize)
  Nothing ->
    GenVariableSize $ \ws -> do
      let s = UV.length ws
          sizeForPartition = s `div` 10
          sizeForValues = s - sizeForPartition
          -- We use the first 10% of the randomness for computing the partition
          (forThePartition, forTheValues) = splitRandomnessAt sizeForPartition ws
      partition <- runGen (genPartition sizeForValues) forThePartition
      pure $ rights $ go forTheValues partition
  where
    go :: Randomness -> [Size] -> [Either String a]
    go rs = \case
      [] -> []
      (s : rest) ->
        let (forThisGen, forTheRest) = splitRandomnessAt s rs
         in runGen gen forThisGen : go forTheRest rest

-- | 'genPartition n' generates a list 'ls' such that 'sum ls' equals 'n', approximately.
genPartition :: Int -> Gen [Int]
genPartition total = GenVariableSize $ \ws ->
  pure $ case UV.length ws of
    0 -> [total]
    1 -> [total]
    _ ->
      let maxLen = UV.length ws - 1
          len = computeIntFromTriangleDistribution 0 maxLen maxLen (ws UV.! 0)
          us = map computeProperFraction (UV.toList (UV.take len (UV.drop 1 ws)))
          invs = map (invE 0.25) us
       in -- Rescale the sizes to (approximately) sum to the given size.
          map (round . (* (fromIntegral total / sum invs))) invs
  where
    -- Use an exponential distribution for generating the
    -- sizes in the partition.
    invE :: Double -> Double -> Double
    invE lambda u = (-log (1 - u)) / lambda

-- | Generate a list length leq the given size for a generator with fixed size
genFixedSizeListLenghtWithMaximum :: Size -> Gen Int
genFixedSizeListLenghtWithMaximum maxLen =
  -- Use a triangle distribution for generating the
  -- length of the list
  -- with minimum length '0', mode length 'maxLen'
  -- and given max length.
  genIntFromTriangleDistribution 0 maxLen maxLen

-- | Generate a list length leq the given size for a generator with variable size
genVariableListLengthWithMaximum :: Size -> Gen Int
genVariableListLengthWithMaximum maxLen =
  -- Use a triangle distribution for generating the
  -- length of the list
  -- with minimum length '0', mode length '2'
  -- and given max length.
  genIntFromTriangleDistribution 0 maxLen 2

genIntFromTriangleDistribution :: Int -> Int -> Int -> Gen Int
genIntFromTriangleDistribution minimal maximal mode =
  computeIntFromTriangleDistribution minimal maximal mode <$> takeNextRandomWord

computeIntFromTriangleDistribution :: Int -> Int -> Int -> RandomWord -> Int
computeIntFromTriangleDistribution minimal maximal mode rw =
  round $ computeDoubleFromTriangleDistribution (fromIntegral minimal) (fromIntegral maximal) (fromIntegral mode) rw

genDoubleFromTriangleDistribution :: Double -> Double -> Double -> Gen Double
genDoubleFromTriangleDistribution minimal maximal mode =
  computeDoubleFromTriangleDistribution minimal maximal mode <$> takeNextRandomWord

-- | Choose a value from a triangel distribution with given minimal value,
-- maximal value, mode, and random Double
--
-- See https://en.wikipedia.org/wiki/Triangular_distribution
computeDoubleFromTriangleDistribution :: Double -> Double -> Double -> RandomWord -> Double
computeDoubleFromTriangleDistribution minimal maximal mode rw =
  let u = computeProperFraction rw
      a = minimal
      b = maximal
      c = mode
      fc = (c - a) / (b - a)
   in if u < fc
        then a + sqrt (u * (b - a) * (c - a))
        else b - sqrt ((1 - u) * (b - a) * (b - c))

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

-- | Compute a randomness vector based on a size and seed
computeRandomness :: Int -> Seed -> Randomness
computeRandomness size seed = computeRandomnessWithSMGen size (mkSMGen seed)

-- | Compute a randomness vector based on a size and splitmix generator
computeRandomnessWithSMGen :: Int -> SMGen -> Randomness
computeRandomnessWithSMGen size = UV.unfoldrExactN size SM.nextWord64

-- Pretend every shrunk version still fails.
computeAllShrinks :: Randomness -> [Randomness]
computeAllShrinks v =
  if UV.null v
    then []
    else
      let vs = shrinkRandomness v
       in case listToMaybe vs of
            Nothing -> []
            Just v' -> v' : computeAllShrinks v'

-- Compute the next shrinking steps.
--
-- Laws:
--
-- 1. Must never shrink to itself.
--
-- These needs to be in order of "must shrinking progress first"
shrinkRandomness :: Randomness -> [Randomness]
shrinkRandomness ws =
  concat
    [ every tryToLog,
      every tryToSqrt,
      every (tryToDivideBy 2),
      every tryToPred,
      shorteningsFromBack,
      shorteningsFromFront,
      each tryToLog,
      each tryToSqrt,
      each (tryToDivideBy 2),
      each tryToPred
    ]
  where
    -- TODO keep a cache of the randomnesses we've
    -- already tried
    every :: (Word64 -> Maybe Word64) -> [Randomness]
    every fun =
      let ws' = UV.map (\v -> fromMaybe v $ fun v) ws
       in [ws' | ws /= ws']
    shorteningsFromBack :: [Randomness]
    shorteningsFromBack = [UV.take l ws | l <- [1 .. (UV.length ws) - 1]]
    shorteningsFromFront :: [Randomness]
    shorteningsFromFront = [UV.drop l ws | l <- [UV.length ws - 1, UV.length ws - 2 .. 1]]
    each :: (Word64 -> Maybe Word64) -> [Randomness]
    each fun =
      mapMaybe
        ( \ix -> do
            let v = UV.unsafeIndex ws ix
            v' <- fun v
            pure $
              UV.modify
                ( \mv ->
                    MUV.write mv ix v'
                )
                ws
        )
        [0 .. UV.length ws - 1]
    tryToLog = \case
      0 -> Nothing
      1 -> Nothing
      w -> Just $ floor (logBase 2 (fromIntegral w) :: Double)
    tryToSqrt = \case
      0 -> Nothing
      1 -> Nothing
      w -> Just $ floor (sqrt (fromIntegral w) :: Double)
    tryToDivideBy d = \case
      0 -> Nothing
      w -> Just $ w `div` d
    tryToPred = \case
      0 -> Nothing
      w -> Just $ pred w

data Property ls where
  PropBool :: Bool -> Property '[]
  PropGen :: Gen a -> (a -> Property ls) -> Property (a ': ls)

data family PList (l :: [Type])

data instance PList '[] = PNil

data instance PList (x ': xs) = x `PCons` PList xs

deriving instance Show (PList '[])

deriving instance (Show x, Show (PList xs)) => Show (PList (x ': xs))

deriving instance Eq (PList '[])

deriving instance (Eq x, Eq (PList xs)) => Eq (PList (x ': xs))

deriving instance Ord (PList '[])

deriving instance (Ord x, Ord (PList xs)) => Ord (PList (x ': xs))

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

class IsProperty ls a | a -> ls where
  toProperty :: a -> Property ls

instance IsProperty ls (Property ls) where
  toProperty = id

instance IsProperty '[] Bool where
  toProperty = PropBool

instance (GenValid a, IsProperty ls b) => IsProperty (a ': ls) (a -> b) where
  toProperty func = forAll genValid $ \a -> func a

forAll :: IsProperty ls prop => Gen a -> (a -> prop) -> Property (a ': ls)
forAll gen func = PropGen gen $ \a -> toProperty (func a)

-- forAllShrink does not exist anymore, yay
