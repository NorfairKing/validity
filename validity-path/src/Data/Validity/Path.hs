{-# LANGUAGE FlexibleInstances #-}

module Data.Validity.Path where

import Data.Validity

import Path
import Path.Internal

import Data.List (isInfixOf)

import qualified System.FilePath as FilePath

instance Validity (Path Abs File) where
    isValid p@(Path fp) =
        FilePath.isAbsolute fp &&
        not (FilePath.hasTrailingPathSeparator fp) &&
        FilePath.isValid fp &&
        not (".." `isInfixOf` fp) && (parseAbsFile fp == Just p)

instance Validity (Path Rel File) where
    isValid p@(Path fp) =
        FilePath.isRelative fp &&
        not (FilePath.hasTrailingPathSeparator fp) &&
        FilePath.isValid fp &&
        fp /= "." &&
        fp /= ".." && not (".." `isInfixOf` fp) && (parseRelFile fp == Just p)

instance Validity (Path Abs Dir) where
    isValid p@(Path fp) =
        FilePath.isAbsolute fp &&
        FilePath.hasTrailingPathSeparator fp &&
        FilePath.isValid fp &&
        not (".." `isInfixOf` fp) && (parseAbsDir fp == Just p)

instance Validity (Path Rel Dir) where
    isValid p@(Path fp) =
        FilePath.isRelative fp &&
        FilePath.hasTrailingPathSeparator fp &&
        FilePath.isValid fp &&
        not (null fp) &&
        fp /= "." &&
        fp /= ".." && not (".." `isInfixOf` fp) && (parseRelDir fp == Just p)
