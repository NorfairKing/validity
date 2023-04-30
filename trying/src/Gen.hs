{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Gen where

import Control.Monad
import Control.Selective
import Data.Kind
import Data.Maybe
import Data.Tuple (swap)
import Data.Validity
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import Data.Word
import Debug.Trace
import GHC.Float (castWord64ToDouble)
import System.Random.SplitMix as SM

-- TODO: Newtype?
type Size = Int

type RandomWord = Word64

-- TODO: Newtype?
type Randomness = Vector Word64

-- Integrated shrinking AND size handling.
--
-- The length of the vector is the size paremeter, in some sense.
-- That's how much randomness the generator is allowed to use.
data Gen a where
  -- | Generator that uses a fixed amount of randomness
  GenFixedSize :: Size -> (Randomness -> a) -> Gen a
  -- | Generator that uses a variable amount of randomness.
  GenVariableSize :: (Randomness -> a) -> Gen a
  -- | Generator that uses the amount of randomness left over to decide what to do.
  GenSized :: (Size -> Gen a) -> Gen a
  -- | For the Functor instance
  GenPure :: a -> Gen a
  -- | For the Applicative instance
  GenFMap :: (a -> b) -> Gen a -> Gen b
  GenAp :: Gen (a -> b) -> Gen a -> Gen b
  -- | For the Selective instance
  GenSelect :: Gen (Either a b) -> Gen (a -> b) -> Gen b
  -- | For the Monad instance
  GenBind :: Gen a -> (a -> Gen b) -> Gen b

instance Functor Gen where
  fmap = GenFMap

instance Applicative Gen where
  pure = GenPure
  (<*>) = GenAp

instance Selective Gen where
  select = GenSelect

instance Monad Gen where
  (>>=) = GenBind

sized :: (Size -> Gen a) -> Gen a
sized = GenSized

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
      GenSelect g1 g2 ->
        -- This may not be the actual size, but is definitely an upper bound
        (+) <$> go g1 <*> go g2
      GenBind _ _ -> Nothing

runGen :: Gen a -> Randomness -> a
runGen = flip go
  where
    go :: Randomness -> Gen a -> a
    go ws = \case
      GenFixedSize size fun -> fun (UV.take (fromIntegral size) ws)
      GenVariableSize fun -> fun ws
      GenSized fun -> go ws (fun (fromIntegral (UV.length ws)))
      GenPure a -> a
      GenFMap f g' -> f $ go ws g'
      GenAp gf ga ->
        -- TODO the way this is called is O(n^2). That can probably be done better.
        let (leftWs, rightWs) = case (sizeOfGen gf, sizeOfGen ga) of
              (Nothing, Nothing) -> computeSplitRandomness ws
              (Just fsize, _) -> splitRandomnessAt (fromIntegral fsize) ws
              (_, Just asize) -> swap $ splitRandomnessAt (fromIntegral asize) ws
         in (go leftWs gf) (go rightWs ga)
      GenSelect gEither gFun ->
        let (leftWs, rightWs) = case (sizeOfGen gEither, sizeOfGen gFun) of
              (Nothing, Nothing) -> computeSplitRandomness ws
              (Just fsize, _) -> splitRandomnessAt (fromIntegral fsize) ws
              (_, Just asize) -> swap $ splitRandomnessAt (fromIntegral asize) ws
            e = go leftWs gEither
         in case e of
              Left a -> go rightWs gFun a
              Right b -> b
      GenBind ga mb ->
        let (leftWs, rightWs) =
              case sizeOfGen ga of
                Nothing -> computeSplitRandomness ws
                Just asize -> splitRandomnessAt (fromIntegral asize) ws
            a = go leftWs ga
         in go rightWs (mb a)

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
        n ->
          let leftSize = computeSplit (pred len) (UV.head ws)
              restRandomness = UV.tail ws
           in UV.splitAt leftSize restRandomness

genFromSingleRandomWord :: (Maybe RandomWord -> a) -> Gen a
genFromSingleRandomWord func = GenFixedSize 1 $ \v ->
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

-- | Generate a 'Double' within a range, shrink to the lower end
genDouble :: (Double, Double) -> Gen Double
genDouble (lo, hi) = do
  w <- takeNextRandomWord
  -- 0: lo
  -- maxBound: hi
  -- n: (hi - lo) / maxBound * w
  pure $ ((hi - lo) / fromIntegral (maxBound :: Word64)) * fromIntegral w

-- | Generate a 'Double' within '[0,1]' and shrink to 0.
genProperFraction :: Gen Double
genProperFraction = genDouble (0, 1)

-- Better splitting with fixed size lists
genListOf :: forall a. Gen a -> Gen [a]
genListOf gen = case sizeOfGen gen of
  Just asize -> sized $ \s -> do
    let maxLen = s `div` asize
    len <- genListLengthWithMaximum maxLen
    GenVariableSize $ \ws -> do
      go ws (replicate len asize)
  Nothing ->
    sized $ \s -> do
      len <- genListLengthWithMaximum (max 0 (s - 1))
      partition <- genPartition len
      GenVariableSize $ \ws ->
        go ws partition
  where
    go :: Randomness -> [Size] -> [a]
    go rs = \case
      [] -> []
      (s : rest) ->
        let (forThisGen, forTheRest) = splitRandomnessAt s rs
         in runGen gen forThisGen : go forTheRest rest

-- | 'genPartition n' generates a list 'ls' such that 'sum ls' equals 'n', approximately.
genPartition :: Int -> Gen [Int]
genPartition = \case
  0 -> pure []
  i -> genListLengthWithMaximum i >>= go i
  where
    go :: Int -> Int -> Gen [Int]
    go size len = do
      us <- replicateM len genProperFraction
      let invs = map (invE 0.25) us
      -- Rescale the sizes to (approximately) sum to the given size.
      pure $ map (round . (* (fromIntegral size / sum invs))) invs

    -- Use an exponential distribution for generating the
    -- sizes in the partition.
    invE :: Double -> Double -> Double
    invE lambda u = (-log (1 - u)) / lambda

-- | Generate a list length leq the given size
genListLengthWithMaximum :: Size -> Gen Int
genListLengthWithMaximum maxLen = computeListLengthWithMaximum maxLen <$> genProperFraction

computeListLengthWithMaximum :: Size -> Double -> Int
computeListLengthWithMaximum maxLen =
  round . invT (fromIntegral maxLen)
  where
    -- Use a triangle distribution for generating the
    -- length of the list
    -- with minimum length '0', mode length '2'
    -- and given max length.
    invT :: Double -> Double -> Double
    invT m u =
      let a = 0
          b = m
          c = 2
          fc = (c - a) / (b - a)
       in if u < fc
            then a + sqrt (u * (b - a) * (c - a))
            else b - sqrt ((1 - u) * (b - a) * (b - c))

-- | Compute a randomness vector based on a size and seed
computeRandomness :: Int -> Word64 -> Randomness
computeRandomness size seed =
  UV.unfoldrExactN size SM.nextWord64 smGen
  where
    smGen = mkSMGen seed

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
      every tryToDivideByTwo,
      -- every tryToPred,
      shorteningsFromBack,
      shorteningsFromFront,
      each tryToLog,
      each tryToSqrt,
      each tryToDivideByTwo -- ,
      -- each tryToPred
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
      w -> Just $ floor $ logBase 2 $ fromIntegral w
    tryToSqrt = \case
      0 -> Nothing
      1 -> Nothing
      w -> Just $ floor $ sqrt $ fromIntegral w
    tryToDivideByTwo = \case
      0 -> Nothing
      w -> Just $ w `div` 2
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

runIsProperty ::
  (Show (PList ls), IsProperty ls prop) =>
  Int ->
  Int ->
  Int ->
  Word64 ->
  prop ->
  Maybe (PList ls) -- Counterexample
runIsProperty successes maxSize maxShrinks seed prop =
  runProperty successes maxSize maxShrinks seed $
    toProperty prop

runProperty ::
  Show (PList ls) =>
  Int ->
  Int ->
  Int ->
  Word64 ->
  Property ls ->
  Maybe (PList ls) -- Counterexample
runProperty successes maxSize maxShrinks seed prop =
  let sizes = computeSizes successes maxSize
   in go $ zip sizes [seed ..]
  where
    go = \case
      [] -> Nothing
      ((size, seed) : rest) ->
        let t@(values, result) = runPropertyOn maxShrinks (computeRandomness size seed) prop
         in if result then go rest else Just values

computeSizes :: Int -> Int -> [Int]
computeSizes successes maxSize = case successes of
  0 -> []
  1 -> [0]
  2 -> [0, maxSize]
  n -> [0] ++ [i * maxSize `div` (n - 1) | i <- [1 .. n - 2]] ++ [maxSize]

runPropertyOn ::
  Show (PList ls) =>
  Int ->
  Randomness ->
  Property ls ->
  (PList ls, Bool)
runPropertyOn maxShrinks ws prop =
  let (values, result) = runPropertyOnce ws prop
   in if result
        then (values, result)
        else case shrinkProperty maxShrinks ws prop of
          Nothing -> (values, result)
          Just values' -> (values', result)

runPropertyOnce ::
  Randomness ->
  Property ls ->
  (PList ls, Bool)
runPropertyOnce = go
  where
    go :: Randomness -> Property ls -> (PList ls, Bool)
    go ws = \case
      PropBool b -> (PNil, b)
      PropGen gen func ->
        let (usedRandomness, restRandomness) = computeSplitRandomness ws
            value = runGen gen usedRandomness
            (generateds, result) = go restRandomness (func value)
         in (PCons value generateds, result)

shrinkProperty ::
  forall ls.
  Show (PList ls) =>
  Int ->
  Randomness ->
  Property ls ->
  Maybe (PList ls)
shrinkProperty maxShrinks r prop =
  case shrinkPropertyAndReturnAllShrinks maxShrinks r prop of
    [] -> Nothing
    ls -> Just $ last ls

-- TODO record how many shrinks were done
shrinkPropertyAndReturnAllShrinks ::
  forall ls.
  Show (PList ls) =>
  Int ->
  Randomness ->
  Property ls ->
  [PList ls]
shrinkPropertyAndReturnAllShrinks maxShrinks r prop = go (traceShow ("At start:", maxShrinks) maxShrinks) r
  where
    run :: Randomness -> (PList ls, Bool)
    run ws' = runPropertyOnce ws' prop
    go :: Int -> Randomness -> [PList ls]
    go currentShrinksLeft ws =
      let (triesDone, shrinks) = traceShow ("Before:", currentShrinksLeft) $ shrinkPropertyOneStep currentShrinksLeft ws prop
          newShrinksLeft = max 0 (currentShrinksLeft - triesDone)
       in traceShow ("After:", newShrinksLeft) $ case listToMaybe shrinks of
            Nothing -> []
            Just (ws', values) -> values : go newShrinksLeft ws'

shrinkPropertyOneStep ::
  Show (PList ls) =>
  Int ->
  Randomness ->
  Property ls ->
  (Int, [(Randomness, PList ls)])
shrinkPropertyOneStep maxShrinksThisRound ws prop =
  let shrunkRandomnesses = take maxShrinksThisRound (shrinkRandomness ws)
      triesDone = length shrunkRandomnesses
   in ( triesDone,
        do
          ws' <- shrunkRandomnesses
          let (vals, result) = runPropertyOnce ws' prop
          guard $ not result
          pure $ traceShowId (ws', vals)
      )

class IsProperty ls a where
  toProperty :: a -> Property ls

instance IsProperty '[] Bool where
  toProperty = PropBool

instance (GenValid a, IsProperty ls b) => IsProperty (a ': ls) (a -> b) where
  toProperty func = PropGen genValid $ \a -> toProperty (func a)
