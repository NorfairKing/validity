{-# OPTIONS_GHC -Wno-orphans #-}

module Data.GenValidity.ByteString where

import Data.GenValidity
import Data.Validity.ByteString ()

import Test.QuickCheck

import Control.Monad

import qualified Data.ByteString as SB
import Data.ByteString (ByteString)

instance GenUnchecked ByteString where
    genUnchecked = SB.pack <$> genUnchecked

instance GenValid ByteString

instance GenInvalid ByteString
