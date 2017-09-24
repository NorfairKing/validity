{-# OPTIONS_GHC -fno-warn-orphans #-}
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
        and
            [ FilePath.isAbsolute fp
            , not (FilePath.hasTrailingPathSeparator fp)
            , FilePath.isValid fp
            , not (".." `isInfixOf` fp)
            , (parseAbsFile fp == Just p)
            ]
    validate p@(Path fp) =
        mconcat
            [ FilePath.isAbsolute fp <?@> "The path is absolute."
            , not (FilePath.hasTrailingPathSeparator fp) <?@>
              "The path has no trailing path separator."
            , FilePath.isValid fp <?@>
              "System.FilePath considers the path valid."
            , not (".." `isInfixOf` fp) <?@> "The path does not contain '..'."
            , (parseAbsFile fp == Just p) <?@>
              "The path can be identically parsed as an absolute file path."
            ]

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
        and
            [ FilePath.isRelative fp
            , not (FilePath.hasTrailingPathSeparator fp)
            , FilePath.isValid fp
            , fp /= "."
            , not (".." `isInfixOf` fp)
            , (parseRelFile fp == Just p)
            ]
    validate p@(Path fp) =
        mconcat
            [ FilePath.isRelative fp <?@> "The path is relative."
            , not (FilePath.hasTrailingPathSeparator fp) <?@>
              "The path has no trailing path separator."
            , FilePath.isValid fp <?@>
              "System.FilePath considers the path valid."
            , fp /= "." <?@> "The path does not equal \".\"."
            , not (".." `isInfixOf` fp) <?@> "The path does not contain '..'."
            , (parseRelFile fp == Just p) <?@>
              "The path can be identically parsed as a relative file path."
            ]

-- | An absolute path to a directory is valid if:
--
-- * Its path is an absolute path
-- * Its path has a trailing path separator
-- * Its path is valid according to 'System.FilePath's definition.
-- * Its path does not contain '..'.
-- * Parsing the path and rendering it again results in the same path.
instance Validity (Path Abs Dir) where
    isValid p@(Path fp) =
        and
            [ FilePath.isAbsolute fp
            , FilePath.hasTrailingPathSeparator fp
            , FilePath.isValid fp
            , not (".." `isInfixOf` fp)
            , (parseAbsDir fp == Just p)
            ]
    validate p@(Path fp) =
        mconcat
            [ FilePath.isAbsolute fp <?@> "The path is absolute."
            , FilePath.hasTrailingPathSeparator fp <?@>
              "The path has a trailing path separator."
            , FilePath.isValid fp <?@>
              "System.FilePath considers the path valid."
            , not (".." `isInfixOf` fp) <?@> "The path does not contain '..'."
            , (parseAbsDir fp == Just p) <?@>
              "The path can be identically parsed as an absolute directory path."
            ]

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
        and
            [ FilePath.isRelative fp
            , FilePath.hasTrailingPathSeparator fp
            , FilePath.isValid fp
            , not (null fp)
            , fp /= "."
            , not (".." `isInfixOf` fp)
            , (parseRelDir fp == Just p)
            ]
    validate p@(Path fp) =
        mconcat
            [ FilePath.isRelative fp <?@> "The path is relattive."
            , FilePath.hasTrailingPathSeparator fp <?@>
              "The path has a trailing path separator."
            , FilePath.isValid fp <?@>
              "System.FilePath considers the path valid."
            , not (null fp) <?@> "The path is not empty."
            , fp /= "." <?@> "The path does not equal \".\"."
            , not (".." `isInfixOf` fp) <?@> "The path does not contain '..'."
            , (parseRelDir fp == Just p) <?@>
              "The path can be identically parsed as a relative directory path."
            ]
