{-# LANGUAGE FlexibleInstances #-}

module Data.GenValidity.Path where

import Data.GenValidity
import Data.Validity.Path ()

import Path
import Path.Internal

instance GenUnchecked (Path a f) where
    genUnchecked = Path <$> genUnchecked

instance GenValid (Path Abs File)

instance GenValid (Path Abs Dir)

instance GenValid (Path Rel File)

instance GenValid (Path Rel Dir)

instance GenInvalid (Path Abs File)

instance GenInvalid (Path Abs Dir)

instance GenInvalid (Path Rel File)

instance GenInvalid (Path Rel Dir)
