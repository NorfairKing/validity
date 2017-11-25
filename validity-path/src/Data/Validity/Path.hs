{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

module Data.Validity.Path where

import Data.Validity

import Path
import Path.Internal

import Data.List ( isInfixOf
#if MIN_VERSION_path(0,6,0)
                 , isSuffixOf
#endif
                 )

import qualified System.FilePath as FilePath


-- | An absolute path to a file is valid if:
--
-- * Its path is an absolute path
-- * Its path has no trailing path separators
-- * Its path is valid according to 'System.FilePath's definition.
#if MIN_VERSION_path(0,6,0)
-- * Its path does not end in '/.'
-- * Its path is not '.'
#endif
-- * Its path does not contain '..'.
-- * Parsing the path and rendering it again results in the same path.
instance Validity (Path Abs File) where
    isValid p@(Path fp) =
        and
            [ FilePath.isAbsolute fp
            , not (FilePath.hasTrailingPathSeparator fp)
            , FilePath.isValid fp
#if MIN_VERSION_path(0,6,0)
            , not ("/." `isSuffixOf` fp)
            , fp /= "."
#endif
            , not (".." `isInfixOf` fp)
            , parseAbsFile fp == Just p
            ]
    validate p@(Path fp) =
        mconcat
            [ FilePath.isAbsolute fp <?@> "The path is absolute."
            , not (FilePath.hasTrailingPathSeparator fp) <?@>
              "The path has no trailing path separator."
            , FilePath.isValid fp <?@>
              "System.FilePath considers the path valid."
#if MIN_VERSION_path(0,6,0)
            , not ("/." `isSuffixOf` fp) <?@> "The path does not end in /."
            , fp /= "." <?@> "The path does not equal \".\""
#endif
            , not (".." `isInfixOf` fp) <?@> "The path does not contain '..'."
            , (parseAbsFile fp == Just p) <?@>
              "The path can be identically parsed as an absolute file path."
            ]

-- | A relative path to a file is valid if:
--
-- * Its path is a relative path
-- * Its path does not have a trailing path separator
-- * Its path is valid according to 'System.FilePath's definition.
#if MIN_VERSION_path(0,6,0)
-- * Its path does not end in '/.'
-- * Its path is not '.'
-- * Its path is not ''
#endif
-- * Its path is not '.'
-- * Its path does not contain '..'.
-- * Parsing the path and rendering it again results in the same path.
instance Validity (Path Rel File) where
    isValid p@(Path fp) =
        and
            [ FilePath.isRelative fp
            , not (FilePath.hasTrailingPathSeparator fp)
            , FilePath.isValid fp
#if MIN_VERSION_path(0,6,0)
            , not ("/." `isSuffixOf` fp)
            , fp /= "."
            , fp /= ""
#endif
            , fp /= "."
            , not (".." `isInfixOf` fp)
            , parseRelFile fp == Just p
            ]
    validate p@(Path fp) =
        mconcat
            [ FilePath.isRelative fp <?@> "The path is relative."
            , not (FilePath.hasTrailingPathSeparator fp) <?@>
              "The path has no trailing path separator."
            , FilePath.isValid fp <?@>
              "System.FilePath considers the path valid."
            , fp /= "." <?@> "The path does not equal \".\"."
#if MIN_VERSION_path(0,6,0)
            , not ("/." `isSuffixOf` fp) <?@> "The path does not end in /."
            , fp /= "." <?@> "The path is not '.'"
            , fp /= "" <?@> "The path is not ''"
#endif
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
            , parseAbsDir fp == Just p
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
#if MIN_VERSION_path(0,6,0)
-- * Its path is not ''
#else
-- * Its path is not '.'
#endif
-- * Its path does not contain '..'.
-- * Parsing the path and rendering it again results in the same path.
instance Validity (Path Rel Dir) where
    isValid p@(Path fp) =
        and
            [ FilePath.isRelative fp
            , FilePath.hasTrailingPathSeparator fp
            , FilePath.isValid fp
            , not (null fp)
#if MIN_VERSION_path(0,6,0)
            , fp /= "."
#else
            , fp /= ""
#endif
            , not (".." `isInfixOf` fp)
            , parseRelDir fp == Just p
            ]
    validate p@(Path fp) =
        mconcat
            [ FilePath.isRelative fp <?@> "The path is relattive."
            , FilePath.hasTrailingPathSeparator fp <?@>
              "The path has a trailing path separator."
            , FilePath.isValid fp <?@>
              "System.FilePath considers the path valid."
            , not (null fp) <?@> "The path is not empty."
#if MIN_VERSION_path(0,6,0)
            , fp /= "." <?@> "The path does not equal \".\"."
#else
            , fp /= "" <?@> "The path does not equal \"\"."
#endif
            , not (".." `isInfixOf` fp) <?@> "The path does not contain '..'."
            , (parseRelDir fp == Just p) <?@>
              "The path can be identically parsed as a relative directory path."
            ]
