{-# OPTIONS_GHC -Wno-orphans #-}

module Data.GenValidity.Sequence where

import Data.GenValidity
import Data.Validity.Sequence ()

import Data.Sequence (Seq)
import qualified Data.Sequence as S

instance GenUnchecked v =>
         GenUnchecked (Seq v) where
    genUnchecked = S.fromList <$> genUnchecked

instance GenValid v =>
         GenValid (Seq v) where
    genValid = S.fromList <$> genValid

instance GenInvalid v =>
         GenInvalid (Seq v) where
    genInvalid = S.fromList <$> genInvalid
