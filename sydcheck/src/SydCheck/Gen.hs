{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Data.Bits
import Data.Char
import Data.Either (rights)
import Data.Int
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio
import Data.Tuple (swap)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
import Data.Word
import GHC.Generics (Generic)
import GHC.Real
import SydCheck.Randomness

data Gen a = Gen
  { -- | Maximum amount of randomness it uses.
    genSize :: !(Maybe Size),
    -- | Parse a value from randomness
    genParse :: !(Randomness -> Either String a)
  }
  deriving (Generic)

genFixedSize :: Size -> (Randomness -> Either String a) -> Gen a
genFixedSize s = Gen (Just s)

genVariableSize :: (Randomness -> Either String a) -> Gen a
genVariableSize = Gen Nothing

instance Functor Gen where
  fmap f gen = gen {genParse = fmap f . genParse gen}

instance Applicative Gen where
  pure a = genFixedSize 0 $ const $ Right a
  genF <*> genA =
    Gen
      { genSize = (+) <$> genSize genF <*> genSize genA,
        genParse = \rs -> do
          let (leftRs, rightRs) = case (genSize genF, genSize genA) of
                (Nothing, Nothing) -> computeSplitRandomness rs
                (Just fsize, _) -> splitRandomnessAt fsize rs
                (_, Just asize) -> swap $ splitRandomnessAt asize rs
          genParse genF leftRs <*> genParse genA rightRs
      }

instance Alternative Gen where
  empty = fail "Alternative.empty"
  (<|>) g1 g2 =
    Gen
      { genSize = max <$> genSize g1 <*> genSize g2,
        genParse = \rs -> genParse g1 rs <|> genParse g2 rs
      }
  many = genListOf
  some gen = NE.toList <$> (genNonEmptyOf gen)

instance Monad Gen where
  genA >>= mkBGen =
    Gen
      { genSize = Nothing,
        genParse = \rs -> do
          let (leftWs, rightWs) =
                case genSize genA of
                  Nothing -> computeSplitRandomness rs
                  Just asize -> splitRandomnessAt asize rs
          a <- genParse genA leftWs
          let genB = mkBGen a
          genParse genB rightWs
      }

instance Selective Gen where
  select genAOrB genMakeB =
    Gen
      { genSize = (+) <$> genSize genAOrB <*> genSize genMakeB,
        genParse = \rs -> do
          let (leftWs, rightWs) = case (genSize genAOrB, genSize genMakeB) of
                (Nothing, Nothing) -> computeSplitRandomness rs
                (Just fsize, _) -> splitRandomnessAt fsize rs
                (_, Just asize) -> swap $ splitRandomnessAt asize rs
          e <- genParse genAOrB leftWs
          case e of
            Left a -> ($ a) <$> genParse genMakeB rightWs
            Right b -> pure b
      }

instance MonadFail Gen where
  fail s = genFixedSize 0 $ const $ Left s

-- | Make a generator based on how much randomness is available.
--
-- Note that the result is always a variable-size generator.
sized :: (Size -> Gen a) -> Gen a
sized func =
  Gen
    { genSize = Nothing,
      genParse = \rs -> genParse (func (sizeRandomness rs)) rs
    }

-- resize is not available
-- scale is not available

runGen :: Gen a -> Randomness -> Either String a
runGen = genParse

runGenUntilSucceeds :: Size -> Seed -> Gen a -> (Randomness, a)
runGenUntilSucceeds initialSize initialSeed gen = go initialSize initialSeed
  where
    go size seed =
      let ws = computeRandomness size seed
       in case runGen gen ws of
            Left _ -> go (succ size) (succ seed)
            Right a -> (ws, a)

genFromSingleRandomWord :: (Maybe RandomWord -> a) -> Gen a
genFromSingleRandomWord func = genFixedSize 1 $ \v ->
  pure $
    func $
      if nullRandomness v
        then Nothing
        else Just $ UV.head $ unRandomness v

takeNextRandomWord :: Gen RandomWord
takeNextRandomWord = genFromSingleRandomWord $ \case
  Nothing -> 0
  Just w -> w

-- | Generate a 'Bool' and shrink to the given bool
genBool :: Bool -> Gen Bool
genBool minimal = genFromSingleRandomWord $ \case
  Nothing -> minimal
  Just 0 -> minimal
  Just w -> even w

-- | Generate an 'Ordering' and shrink to the given ordering
genOrdering :: Ordering -> Gen Ordering
genOrdering minimal = genFromSingleRandomWord $ \case
  Nothing -> minimal
  Just 0 -> minimal
  Just w -> case w `mod` 3 of
    0 -> LT
    1 -> EQ
    2 -> GT
    _ -> minimal

genChar :: (Char, Char) -> Gen Char
genChar (lo, hi) = chr <$> genInt (ord lo, ord hi)

-- | Run one of the following elements with corresponding frequency and shrink
-- to the first.
frequency :: [(Int, Gen a)] -> Gen a
frequency [] = fail "TODO LOCATION.frequency: called with empty list"
frequency ls = do
  let total = sum $ map fst ls
  case maximum <$> mapM (genSize . snd) ls of
    Nothing -> do
      ix <- genInt (1, total)
      pick ix ls
    Just maxSize -> genFixedSize (maxSize + 1) $ \ws -> case sizeRandomness ws of
      0 -> runGen (snd (head ls)) ws
      1 -> runGen (snd (head ls)) ws
      _ -> do
        ix <- runGen (genInt (1, total)) (takeRandomness 1 ws)
        runGen (pick ix ls) (dropRandomness 1 ws)
  where
    pick :: Int -> [(Int, Gen a)] -> Gen a
    pick n ((k, x) : xs)
      | n <= k = x
      | otherwise = pick (n - k) xs
    pick _ [] = fail "TODO LOCATION.frequency: pick used with empty list"

-- | Run one of the following generators and shrink to the first.
oneof :: [Gen a] -> Gen a
oneof [] = fail "TODO LOCATION.oneof: called with empty list."
oneof ls = do
  -- Not all are fixed-size
  case maximum <$> mapM genSize ls of
    Nothing -> do
      ix <- genInt (0, length ls - 1)
      ls !! ix
    Just maxSize -> genFixedSize (maxSize + 1) $ \ws -> case sizeRandomness ws of
      0 -> runGen (head ls) ws
      1 -> runGen (head ls) ws
      _ -> do
        ix <- runGen (genInt (0, length ls - 1)) (takeRandomness 1 ws)
        runGen (ls !! ix) (dropRandomness 1 ws)

-- | Generate one of the following elements and shrink to the first.
elements :: [a] -> Gen a
elements ls =
  (ls !!) <$> genInt (0, length ls - 1)

-- | Generates a value that satisfies a predicate.
--
-- Note that this makes any generator variable-size.
suchThat :: forall a. Gen a -> (a -> Bool) -> Gen a
suchThat gen predicate = Gen {genSize = Nothing, genParse = go}
  where
    go :: Randomness -> Either String a
    go rs = do
      let (forThisTry, forTheNextTries) = case genSize gen of
            Nothing -> computeSplitRandomness rs
            Just size -> splitRandomnessAt size rs
      a <- genParse gen forThisTry
      if predicate a
        then Right a
        else
          if nullRandomness forThisTry
            then Left "SydCheck.Gen.suchThat: predicate rejected the value."
            else go forTheNextTries

-- | Generates a value for which the given function returns a 'Just', and then
-- returns that value.
--
-- Note that this makes any generator variable-size.
suchThatMap :: forall a b. Gen a -> (a -> Maybe b) -> Gen b
suchThatMap gen fun = Gen {genSize = Nothing, genParse = go}
  where
    go :: Randomness -> Either String b
    go rs = do
      let (forThisTry, forTheNextTries) = case genSize gen of
            Nothing -> computeSplitRandomness rs
            Just size -> splitRandomnessAt size rs
      a <- genParse gen forThisTry
      case fun a of
        Just b -> Right b
        Nothing -> do
          if nullRandomness forThisTry
            then Left "SydCheck.Gen.suchThatMap: Failed to map the value."
            else go forTheNextTries

-- | Tries to generate a value that satisfies a predicate.
--
-- Return 'Nothing' if that fails.
suchThatMaybe :: Gen a -> (a -> Bool) -> Gen (Maybe a)
suchThatMaybe gen predicate = do
  a <- gen
  pure $ do
    guard $ predicate a
    pure a

-- | Generate an 'Int' within a range, shrink to the lower end
genInt :: (Int, Int) -> Gen Int
genInt (lo, hi) = computeInt (lo, hi) <$> takeNextRandomWord

genIntegralAroundZero :: Integral a => Gen a
genIntegralAroundZero =
  oneof
    [ genIntegralAroundZeroPositive,
      genIntegralAroundZeroNegative
    ]

genIntegralAroundZeroNegative :: Integral a => Gen a
genIntegralAroundZeroNegative = genFromSingleRandomWord $ \case
  Nothing -> 0
  Just 0 -> 0
  Just w -> negate $ round (computeDoubleFromExponentialDistribution 0.5 w)

genIntegralAroundZeroPositive :: Integral a => Gen a
genIntegralAroundZeroPositive = genFromSingleRandomWord $ \case
  Nothing -> 0
  Just 0 -> 0
  Just w -> round (computeDoubleFromExponentialDistribution 0.5 w)

genIntegralAroundMinbound :: (Integral a, Bounded a) => Gen a
genIntegralAroundMinbound = genFromSingleRandomWord $ \case
  Nothing -> minBound
  Just 0 -> minBound
  Just w -> minBound + round (computeDoubleFromExponentialDistribution 0.5 w)

genIntegralAroundMaxbound :: (Integral a, Bounded a) => Gen a
genIntegralAroundMaxbound = genFromSingleRandomWord $ \case
  Nothing -> maxBound
  Just 0 -> maxBound
  Just w -> maxBound - round (computeDoubleFromExponentialDistribution 0.5 w)

genUniformInt :: forall a. (Integral a, Bounded a) => Gen a
genUniformInt = genFromSingleRandomWord $ \case
  Nothing -> 0
  Just 0 -> 0
  Just w ->
    if fromIntegral (maxBound :: a) == (maxBound :: Int64)
      then fromIntegral w
      else fromIntegral (w `quot` (fromIntegral (maxBound :: a)))

-- | Generate Int, Int8, Int16, Int32 and Int64 values smartly.
--
-- * Some at the border
-- * Some around zero
-- * Mostly uniformly
genIntX :: forall a. (Integral a, Bounded a) => Gen a
genIntX =
  frequency
    [ (1, genIntegralAroundZeroPositive),
      (1, genIntegralAroundZeroNegative),
      (6, genUniformInt),
      (1, genIntegralAroundMinbound),
      (1, genIntegralAroundMaxbound)
    ]

-- | Generate a 'Word' within a range, shrink to the lower end
genWord :: (Word, Word) -> Gen Word
genWord (lo, hi) = computeWord (lo, hi) <$> takeNextRandomWord

genUniformWord :: forall a. (Integral a, Bounded a) => Gen a
genUniformWord = genFromSingleRandomWord $ \case
  Nothing -> 0
  Just 0 -> 0
  Just w ->
    if fromIntegral (maxBound :: a) == (maxBound :: Word64)
      then fromIntegral w
      else fromIntegral (w `quot` (fromIntegral (maxBound :: a)))

-- | Generate Word, Word8, Word16, Word32 and Word64 values smartly.
--
-- * Some at the border
-- * Some around zero
-- * Mostly uniformly
genWordX :: forall a. (Integral a, Bounded a) => Gen a
genWordX =
  frequency
    [ (1, genIntegralAroundZeroPositive),
      (8, genUniformWord),
      (1, genIntegralAroundMaxbound)
    ]

genUniformFloatX :: Real a => (Word64 -> a) -> Gen a
genUniformFloatX func = genFromSingleRandomWord $ maybe 0 func

genSmallFloatX :: (Real a, RealFloat a) => Gen a
genSmallFloatX = genFromSingleRandomWord $ \case
  Nothing -> 0
  Just w ->
    let (w1, w2) = splitWord64 w
     in fromIntegral w1 / fromIntegral w2

genFloatXAroundBounds :: forall a. (Real a, RealFloat a) => Gen a
genFloatXAroundBounds = genFixedSize 1 $ \rs -> case sizeRandomness rs of
  0 -> pure 0
  _ -> do
    let w :: Word64
        w = UV.head $ unRandomness rs
    let choice = shiftR w 62
    let (significandChoice, exponentChoice) = case choice of
          0 -> (False, False)
          1 -> (False, True)
          2 -> (True, False)
          3 -> (True, True)
          _ -> (False, False)
    let (w1, w2) = splitWord64 (shiftR (shiftL w 2) 2)
    diffExponent <- runGen genIntegralAroundZeroPositive (Randomness (UV.singleton (fromIntegral w1)))
    diffSignificand <- runGen genIntegralAroundZeroPositive (Randomness (UV.singleton (fromIntegral w2)))
    let e =
          if exponentChoice
            then lowerExponent + diffExponent
            else upperExponent - diffExponent
    let s =
          if significandChoice
            then lowerSignificand + diffSignificand
            else upperSignificand - diffSignificand
    pure $ encodeFloat (fromIntegral s) e
  where
    upperSignificand :: Int
    upperSignificand = fromIntegral $ floatRadix (0.0 :: a) ^ floatDigits (0.0 :: a)
    lowerSignificand :: Int
    lowerSignificand = -upperSignificand
    lowerExponent, upperExponent :: Int
    (lowerExponent, upperExponent) = floatRange (0.0 :: a)

-- | Generate floating point numbers smartly:
--
-- * Some denormalised
-- * Some around zero
-- * Some around the bounds
-- * Mostly uniformly via the bitrepresentation
--
-- The function parameter is to go from the bitrepresentation to the floating point value.
genFloatX ::
  forall a.
  (Read a, RealFloat a) =>
  (Word64 -> a) ->
  Gen a
genFloatX func =
  frequency
    [ (24, genUniformFloatX func),
      (1, pure (read "NaN")),
      (1, pure (read "Infinity")),
      (1, pure (read "-Infinity")),
      (1, pure (read "-0")),
      (4, genSmallFloatX),
      (4, genFloatXAroundBounds)
    ]

computeInt :: (Int, Int) -> RandomWord -> Int
computeInt (lo, hi) rw =
  -- TODO this probably fails for very large integers because of double precision (?)
  round $ computeDouble (fromIntegral lo, fromIntegral hi) rw

computeWord :: (Word, Word) -> RandomWord -> Word
computeWord (lo, hi) rw =
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

genRatioOf :: (Integral a) => Gen a -> Gen (Ratio a)
genRatioOf gen =
  let fixDenominator d = case d of
        0 -> 1
        i ->
          let a = abs i
           in if a < 0
                then a - 1 -- a was minbound of a fixed-size number, now it's maxbound.
                else a
   in case genSize gen of
        Nothing -> genVariableSize $ \rs -> do
          let (forNumerator, forDenominator) = computeSplitRandomness rs
          numer <- genParse gen forNumerator
          denom <- fixDenominator <$> genParse gen forDenominator
          pure $ numer :% denom
        Just size -> genFixedSize (2 * size) $ \rs -> do
          let (forNumerator, forDenominator) = splitRandomnessAt size rs
          numer <- genParse gen forNumerator
          denom <- fixDenominator <$> genParse gen forDenominator
          pure $ numer :% denom

-- | Generate a 'Maybe' and shrink to Nothing
genMaybeOf :: Gen a -> Gen (Maybe a)
genMaybeOf gen = frequency [(1, pure Nothing), (3, Just <$> gen)]

-- | Generate an 'Either' and shrink to the 'Left'
genEitherOf :: Gen a -> Gen b -> Gen (Either a b)
genEitherOf = genEitherToLeft

-- | Generate an 'Either' and shrink to the 'Left'
genEitherToLeft :: Gen a -> Gen b -> Gen (Either a b)
genEitherToLeft genLeft genRight =
  oneof
    [ Left <$> genLeft,
      Right <$> genRight
    ]

-- | Generate an 'Either' and shrink to the 'Right'
genEitherToRight :: Gen a -> Gen b -> Gen (Either a b)
genEitherToRight genLeft genRight =
  oneof
    [ Right <$> genRight,
      Left <$> genLeft
    ]

genListOf :: forall a. Gen a -> Gen [a]
genListOf gen = case genSize gen of
  Just gensize -> case gensize of
    0 -> genFromSingleRandomWord $ \case
      Nothing -> []
      Just w ->
        -- TODO 256 here is magic
        let len = computeIntFromTriangleDistribution 0 256 2 w
         in -- TODO is using rights here a good idea?
            rights $ replicate len (runGen gen emptyRandomness)
    Size asize ->
      genVariableSize $ \ws -> case sizeRandomness ws of
        0 -> pure []
        1 -> pure $ rights [runGen gen ws]
        Size s -> do
          let maxLen = (s - 1) `div` asize
              -- Use longer lists where possible, but not always
              len = computeIntFromTriangleDistribution 0 maxLen maxLen (UV.head (unRandomness ws))
          -- TODO is using rights here a good idea?
          pure $ rights $ go (dropRandomness 1 ws) (replicate len (Size asize))
  Nothing ->
    genVariableSize $ \ws -> do
      let Size s = sizeRandomness ws
          -- We use the first 10% of the randomness for computing the partition
          sizeForPartition = s `div` 10
          (forThePartition, forTheValues) = splitRandomnessAt (Size sizeForPartition) ws
          sizeForValues = s - sizeForPartition
          genSizePartition = map Size <$> genPartition sizeForValues
      partition <- runGen genSizePartition forThePartition
      -- TODO is using rights here a good idea?
      pure $ rights $ go forTheValues partition
  where
    go :: Randomness -> [Size] -> [Either String a]
    go rs = \case
      [] -> []
      [_] -> [runGen gen rs]
      (s : rest) ->
        let (forThisGen, forTheRest) = splitRandomnessAt s rs
         in runGen gen forThisGen : go forTheRest rest

genNonEmptyOf :: forall a. Gen a -> Gen (NonEmpty a)
genNonEmptyOf gen = case genSize gen of
  Just gensize -> genVariableSize $ \ws ->
    let (forFirst, forRest) = splitRandomnessAt gensize ws
     in (:|)
          <$> runGen gen forFirst
          <*> runGen (genListOf gen) forRest
  Nothing -> genVariableSize $ \ws -> do
    let Size s = sizeRandomness ws
        -- We use the first 10% of the randomness for computing the partition
        sizeForPartition = s `div` 10
        (forThePartition, forTheValues) = splitRandomnessAt (Size sizeForPartition) ws
        sizeForValues = s - sizeForPartition
        genSizePartition = NE.map Size <$> genNonEmptyPartition sizeForValues
    partition <- runGen genSizePartition forThePartition
    -- TODO is using rights here a good idea?
    go forTheValues partition
  where
    go :: Randomness -> NonEmpty Size -> Either String (NonEmpty a)
    go rs = \case
      (s :| rest) -> case NE.nonEmpty rest of
        Nothing -> (:| []) <$> runGen gen rs
        Just ne ->
          let (forThisGen, forTheRest) = splitRandomnessAt s rs
           in case runGen gen forThisGen of
                Left _ -> go forTheRest ne
                Right v -> (v NE.<|) <$> go forTheRest ne

genMapOf :: Ord k => Gen (k, v) -> Gen (Map k v)
genMapOf g = Map.fromList <$> genListOf g

genVectorOf :: Gen a -> Gen (V.Vector a)
genVectorOf g = V.fromList <$> genListOf g

genStorableVectorOf :: SV.Storable a => Gen a -> Gen (SV.Vector a)
genStorableVectorOf g = SV.fromList <$> genListOf g

genUnboxedVectorOf :: UV.Unbox a => Gen a -> Gen (UV.Vector a)
genUnboxedVectorOf g = UV.fromList <$> genListOf g

-- | 'genPartition n' generates a list 'ls' such that 'sum ls' equals 'n', approximately.
--
-- TODO Refactor this out specifically for sizes
genPartition :: Int -> Gen [Int]
genPartition total = genVariableSize $ \ws ->
  pure $ case sizeRandomness ws of
    0 -> []
    1 -> [total]
    _ ->
      let Size maxLen = sizeRandomness ws - 1
          len = computeIntFromTriangleDistribution 0 maxLen maxLen (UV.head (unRandomness ws))
          invs =
            map
              (computeDoubleFromExponentialDistribution 0.25)
              (UV.toList (unRandomness (takeRandomness (Size len) (dropRandomness 1 ws))))
       in -- Rescale the sizes to (approximately) sum to the given size.
          map (round . (* (fromIntegral total / sum invs))) invs

-- | 'genPartition n' generates a list 'ls' such that 'sum ls' equals 'n', approximately.
--
-- TODO Refactor this out specifically for sizes
genNonEmptyPartition :: Int -> Gen (NonEmpty Int)
genNonEmptyPartition total = genVariableSize $ \ws ->
  pure $ case sizeRandomness ws of
    0 -> total :| []
    1 -> total :| []
    _ ->
      let Size maxLen = sizeRandomness ws - 1
          len = computeIntFromTriangleDistribution 1 maxLen maxLen (UV.head (unRandomness ws))
       in case NE.nonEmpty (UV.toList (unRandomness (takeRandomness (Size len) (dropRandomness 1 ws)))) of
            Nothing -> total :| []
            Just ne ->
              let invs =
                    NE.map
                      (computeDoubleFromExponentialDistribution 0.25)
                      ne
               in -- Rescale the sizes to (approximately) sum to the given size.
                  NE.map (round . (* (fromIntegral total / sum invs))) invs

genDoubleFromExponentialDistribution :: Double -> Gen Double
genDoubleFromExponentialDistribution lambda = genFromSingleRandomWord $ \case
  Nothing -> 0
  Just w -> computeDoubleFromExponentialDistribution lambda w

computeDoubleFromExponentialDistribution :: Double -> RandomWord -> Double
computeDoubleFromExponentialDistribution lambda rw =
  let u = computeProperFraction rw
   in (-log (1 - u)) / lambda

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
