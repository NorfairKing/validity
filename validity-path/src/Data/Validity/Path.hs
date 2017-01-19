{-# LANGUAGE FlexibleInstances #-}

module Data.Validity.Path where

import Data.Validity

import Path
import Path.Internal

import Data.List (isInfixOf)

import qualified System.FilePath as FilePath

-- | An absolute path to a file is valid if:
--
-- * Its path is an absolute path
-- * Its path does not have a trailing path separator
-- * Its path is valid according to 'System.FilePath's definition.
-- * Its path does not contain '..'.
-- * Parsing the path and rendering it again results in the same path.
instance Validity (Path Abs File) where
    isValid p@(Path fp) =
        FilePath.isAbsolute fp &&
        not (FilePath.hasTrailingPathSeparator fp) &&
        FilePath.isValid fp &&
        not (".." `isInfixOf` fp) && (parseAbsFile fp == Just p)

-- | A relative path to a file is valid if:
--
-- * Its path is a relative path
-- * Its path does not have a trailing path separator
-- * Its path is valid according to 'System.FilePath's definition.
-- * Its path is not '.'
-- * Its path does not contain '..'.
-- * Parsing the path and rendering it again results in the same path.
instance Validity (Path Rel File) where
    isValid p@(Path fp) =
        FilePath.isRelative fp &&
        not (FilePath.hasTrailingPathSeparator fp) &&
        FilePath.isValid fp &&
        fp /= "." && not (".." `isInfixOf` fp) && (parseRelFile fp == Just p)

-- | An absolute path to a directory is valid if:
--
-- * Its path is an absolute path
-- * Its path has a trailing path separator
-- * Its path is valid according to 'System.FilePath's definition.
-- * Its path does not contain '..'.
-- * Parsing the path and rendering it again results in the same path.
instance Validity (Path Abs Dir) where
    isValid p@(Path fp) =
        FilePath.isAbsolute fp &&
        FilePath.hasTrailingPathSeparator fp &&
        FilePath.isValid fp &&
        not (".." `isInfixOf` fp) && (parseAbsDir fp == Just p)

-- | A relative path to a directory is valid if:
--
-- * Its path is a relative path
-- * Its path has a trailing path separator
-- * Its path is valid according to 'System.FilePath's definition.
-- * Its path is not '.'
-- * Its path does not contain '..'.
-- * Parsing the path and rendering it again results in the same path.
instance Validity (Path Rel Dir) where
    isValid p@(Path fp) =
        FilePath.isRelative fp &&
        FilePath.hasTrailingPathSeparator fp &&
        FilePath.isValid fp &&
        not (null fp) &&
        fp /= "." && not (".." `isInfixOf` fp) && (parseRelDir fp == Just p)
