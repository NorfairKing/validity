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
import Data.Char
import Data.Either (rights)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
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
  GenFMap ::
    -- The size, so it doesn't have to be computed again.
    -- Must be equal to sizeOfGen of the contained gen
    Maybe Size ->
    (a -> b) ->
    Gen a ->
    Gen b
  GenAp ::
    -- The size, so it doesn't have to be computed again.
    -- Must be equal to the sum of the sizeOfGen of the contained gens
    Maybe Size ->
    Gen (a -> b) ->
    Gen a ->
    Gen b
  -- | For the Alternative instance
  GenAlt ::
    -- The size, so it doesn't have to be computed again.
    -- Must be equal to the maximum of the sizeOfGen of the contained gens
    Maybe Size ->
    Gen a ->
    Gen b ->
    Gen (Either a b)
  -- | For the Selective instance
  GenSelect ::
    -- The size, so it doesn't have to be computed again.
    -- Must be equal to the sum of the sizes of the contained gens
    Maybe Size ->
    Gen (Either a b) ->
    Gen (a -> b) ->
    Gen b
  -- | For the Monad instance
  GenBind :: Gen a -> (a -> Gen b) -> Gen b
  -- | For MonadFail
  GenFail :: String -> Gen a

-- | Map the result of a 'Gen'
--
-- We have some trouble with laws.
--
-- Functor law 1:
--
-- > fmap id == id
--
-- This cannot hold with the `Gen` structure, strictly, but it'll have to morally instead.
--
-- Functor law 2:
--
-- > fmap (f . g) = fmap f . fmap g
--
-- We can make this hold with a special case
instance Functor Gen where
  fmap f = \case
    -- \| Special case for the second functor law.
    GenFMap ms g g' -> GenFMap ms (f . g) g'
    g -> GenFMap (sizeOfGen g) f g

-- | Applicative 'Gen'
--
-- We have more trouble with the laws.
--
-- Further, any definition must satisfy the following:
--
-- Applicative law: Identity
--
-- > pure id <*> v = v
--
-- We can make this hold with a special case.
--
--
-- Applicative law: Composition
--
-- > pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
--
-- We can't make this hold, strictly, so we will have to make it work morally.
--
--
-- Applicative law: Homomorphism
--
-- > pure f <*> pure x = pure (f x)
--
-- We can make this hold with a special case.
--
--
-- Applicative law: Interchange
--
-- > u <*> pure y = pure ($ y) <*> u
--
-- We can make this hold with a special case.
instance Applicative Gen where
  pure = GenPure

  -- Special case for the Homomorphism law.
  GenPure f <*> GenPure a = GenPure (f a)
  -- Special case for the Interchange law
  g1 <*> GenPure a = ($ a) <$> g1
  -- Special case for the Identity law
  GenPure f <*> g2 = f <$> g2
  g1 <*> g2 =
    GenAp
      ((+) <$> sizeOfGen g1 <*> sizeOfGen g2)
      g1
      g2

instance Alternative Gen where
  empty = GenFail "Alternative.empty"
  (<|>) g1 g2 = either id id <$> GenAlt (max <$> sizeOfGen g1 <*> sizeOfGen g2) g1 g2
  many = genListOf
  some gen = NE.toList <$> (genNonEmptyOf gen)

instance Selective Gen where
  select (GenPure aOrB) (GenPure makeB) = pure $ case aOrB of
    Left a -> makeB a
    Right b -> b
  select (GenPure aOrB) genB = case aOrB of
    Left a -> ($ a) <$> genB
    Right b -> pure b
  select genAOrB (GenPure makeB) =
    either makeB id <$> genAOrB
  select g1 g2 = GenSelect ((+) <$> sizeOfGen g1 <*> sizeOfGen g2) g1 g2

instance Monad Gen where
  -- Special case for the Left Identity law
  GenPure a >>= makeBGen = makeBGen a
  mkA >>= makeBGen = GenBind mkA makeBGen

instance MonadFail Gen where
  fail = GenFail

-- | Make a generator based on how much randomness is available.
--
-- Note that the result is always a variable-size generator.
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
      GenFMap ms _ _ -> ms
      GenAp ms _ _ -> ms
      GenAlt ms _ _ -> ms
      GenSelect ms _ _ ->
        -- This may not be the actual size, but is definitely an upper bound
        ms
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
      GenFMap _ f g' -> f <$> go ws g'
      GenAp _ gf ga ->
        -- TODO the way this is called is O(n^2). That can probably be done better.
        let (leftWs, rightWs) = case (sizeOfGen gf, sizeOfGen ga) of
              (Nothing, Nothing) -> computeSplitRandomness ws
              (Just fsize, _) -> splitRandomnessAt fsize ws
              (_, Just asize) -> swap $ splitRandomnessAt asize ws
         in go leftWs gf <*> go rightWs ga
      GenAlt _ g1 g2 -> (Left <$> go ws g1) <|> (Right <$> go ws g2)
      GenSelect _ gEither gFun -> do
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

-- | Generate an 'Int' within a range, shrink to the lower end
genInt :: (Int, Int) -> Gen Int
genInt (lo, hi) = computeInt (lo, hi) <$> takeNextRandomWord

-- | Generate a 'Word' within a range, shrink to the lower end
genWord :: (Word, Word) -> Gen Word
genWord (lo, hi) = computeWord (lo, hi) <$> takeNextRandomWord

-- | Generate Word, Word8, Word16, Word32 and Word64 values smartly.
--
-- * Some at the border
-- * Some around zero
-- * Mostly uniformly
genWordX :: forall a. (Integral a, Bounded a) => Gen a
genWordX =
  frequency
    [ (1, small),
      (1, extreme),
      (8, uniformWord)
    ]
  where
    extreme :: Gen a
    extreme = genFromSingleRandomWord $ \case
      Nothing -> maxBound
      Just 0 -> maxBound
      Just w -> maxBound - round (logBase 2 (fromIntegral w :: Double))
    small :: Gen a
    small = genFromSingleRandomWord $ \case
      Nothing -> 0
      Just 0 -> minBound
      Just w -> 0 + round (logBase 2 (fromIntegral w :: Double))
    uniformWord :: Gen a
    uniformWord = genFromSingleRandomWord $ \case
      Nothing -> 0
      Just 0 -> 0
      Just w -> fromIntegral (w `quot` (fromIntegral (maxBound :: a)))

-- uniformWord :: Gen a
-- uniformWord = genWord (minBound, maxBound)

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
genListOf gen = case sizeOfGen gen of
  Just gensize -> case gensize of
    0 -> genFromSingleRandomWord $ \case
      Nothing -> []
      Just w ->
        -- TODO 256 here is magic
        let len = computeIntFromTriangleDistribution 0 256 2 w
         in -- TODO is using rights here a good idea?
            rights $ replicate len (runGen gen emptyRandomness)
    Size asize ->
      GenVariableSize $ \ws -> case sizeRandomness ws of
        0 -> pure []
        1 -> pure $ rights [runGen gen ws]
        Size s -> do
          let maxLen = (s - 1) `div` asize
              -- Use longer lists where possible, but not always
              len = computeIntFromTriangleDistribution 0 maxLen maxLen (UV.head (unRandomness ws))
          -- TODO is using rights here a good idea?
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
genNonEmptyOf gen = case sizeOfGen gen of
  Just gensize -> GenVariableSize $ \ws ->
    let (forFirst, forRest) = splitRandomnessAt gensize ws
     in (:|)
          <$> runGen gen forFirst
          <*> runGen (genListOf gen) forRest
  Nothing -> GenVariableSize $ \ws -> do
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

-- | 'genPartition n' generates a list 'ls' such that 'sum ls' equals 'n', approximately.
--
-- TODO Refactor this out specifically for sizes
genPartition :: Int -> Gen [Int]
genPartition total = GenVariableSize $ \ws ->
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
genNonEmptyPartition total = GenVariableSize $ \ws ->
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
