{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SydCheck.Gen where

import Control.Applicative
import Control.Monad
import Control.Selective
import Data.Either (rights)
import Data.Tuple (swap)
import qualified Data.Vector.Unboxed as UV
import Data.Word
import SydCheck.Randomness

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
      GenFixedSize size fun -> fun (takeRandomness size ws)
      GenVariableSize fun -> fun ws
      GenSized fun -> go ws (fun (sizeRandomness ws))
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

genBool :: Bool -> Gen Bool
genBool minimal = genFromSingleRandomWord $ \case
  Nothing -> minimal
  Just w -> even w

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
  Just (Size asize) ->
    GenVariableSize $ \ws -> case UV.length ws of
      0 -> pure []
      1 -> pure $ rights [runGen gen ws]
      _ -> do
        let Size s = sizeRandomness ws
            maxLen = (s - 1) `div` asize
            -- Use longer lists where possible, but not always
            len = computeIntFromTriangleDistribution 0 maxLen maxLen (ws UV.! 0)
        pure $ rights $ go (dropRandomness 1 ws) (replicate len (Size asize))
  Nothing ->
    GenVariableSize $ \ws -> do
      let Size s = sizeRandomness ws
          -- We use the first 10% of the randomness for computing the partition
          sizeForPartition = s `div` 10
          (forThePartition, forTheValues) = splitRandomnessAt (Size sizeForPartition) ws
          sizeForValues = s - sizeForPartition
          genSizePartition = map Size <$> genPartition sizeForValues
      partition <- runGen genSizePartition forThePartition
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
genFixedSizeListLenghtWithMaximum :: Int -> Gen Int
genFixedSizeListLenghtWithMaximum maxLen =
  -- Use a triangle distribution for generating the
  -- length of the list
  -- with minimum length '0', mode length 'maxLen'
  -- and given max length.
  genIntFromTriangleDistribution 0 maxLen maxLen

-- | Generate a list length leq the given size for a generator with variable size
genVariableListLengthWithMaximum :: Int -> Gen Int
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
