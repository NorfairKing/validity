{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.ByteString where

import Data.GenValidity
import Data.Validity.ByteString ()

import Test.QuickCheck

import Control.Monad
#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
#endif
import qualified Data.ByteString as SB
import Data.ByteString (ByteString)

instance GenUnchecked ByteString where
    genUnchecked = SB.pack <$> genUnchecked

instance GenValid ByteString

instance GenInvalid ByteString
