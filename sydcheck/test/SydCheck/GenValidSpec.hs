{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SydCheck.GenValidSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Word
import SydCheck.Gen
import SydCheck.GenValid
import SydCheck.Randomness
import SydCheck.Shrinking
import Test.Syd

spec :: Spec
spec = do
  describe "runGen" $ do
    goldenGenValidSpec @() "unit"
    goldenGenValidSpec @Bool "bool"
    goldenGenValidSpec @Ordering "ordering"
    goldenGenValidSpec @Word8 "word8"
    goldenGenValidSpec @Char "char"
    goldenGenValidSpec @(Word8, Word8) "tuple-word8-word8"
    goldenGenValidSpec @(Maybe Word8) "maybe-word8"
    goldenGenValidSpec @(Either Word8 Word8) "either-word8-word8"
    goldenGenValidSpec @[Word8] "list-word8"
    goldenGenValidSpec @[()] "list-unit"
    goldenGenValidSpec @(NonEmpty Word8) "nonempty-word8"
    goldenGenValidSpec @(NonEmpty ()) "nonempty-unit"
    goldenGenValidSpec @(NonEmpty (NonEmpty Word8)) "nonempty-nonempty-word8"
    goldenGenValidSpec @[[Word8]] "list-list-word8"
    goldenGenValidSpec @Word64 "word64"

goldenGenValidSpec :: forall a. (Show a, GenValid a) => FilePath -> Spec
goldenGenValidSpec = goldenGenSpec (genValid @a)

goldenGenSpec :: forall a. (Show a) => Gen a -> FilePath -> Spec
goldenGenSpec gen fp = do
  let nbValues = 100
      maxSize = nbValues
      sizes = [0 .. maxSize]
      seeds = [42 ..]
  it ("generates the same " <> fp <> " values") $ do
    let randomnesses = zipWith computeRandomness sizes seeds
        values :: [Either String a]
        values = map (runGen gen) randomnesses
    pureGoldenStringFile ("test_resources/gen/" <> fp <> ".txt") $
      unlines $
        map (either id show) values
  it ("shrinks to the same " <> fp <> " values") $ do
    let (randomness, maxValue) = runGenUntilSucceeds maxSize 42 gen
        shrinks :: [Either String a]
        shrinks = map (runGen gen) $ take 10 $ computeAllShrinks randomness
    pureGoldenStringFile ("test_resources/shrink/" <> fp <> ".txt") $
      unlines $
        show maxValue
          : ""
          : map (either id show) shrinks
