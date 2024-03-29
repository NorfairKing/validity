{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Path where

import Data.List (isInfixOf, isSuffixOf)
import Data.Validity
import Path
import Path.Internal
import qualified System.FilePath as FilePath

-- | An absolute path to a file is valid if:
--
-- * Its path is an absolute path
-- * Its path has no trailing path separators
-- * Its path is valid according to 'System.FilePath's definition.
-- * Its path does not end in '/.'
-- * Its path is not '.'
-- * Its path does not contain '..'.
-- * The path contains no UTF16 Surrogate codepoints
-- * Parsing the path and rendering it again results in the same path.
instance Validity (Path Abs File) where
  validate p@(Path fp) =
    mconcat
      [ declare "The path is absolute." $ FilePath.isAbsolute fp,
        declare "The path has no trailing path separator." $
          not (FilePath.hasTrailingPathSeparator fp),
        declare "System.FilePath considers the path valid." $
          FilePath.isValid fp,
        declare "The path does not end in /." $ not ("/." `isSuffixOf` fp),
        declare "The path does not equal \".\"" $ fp /= ".",
        declare "The path does not contain '..'." $ not (".." `isInfixOf` fp),
        decorate "The path contains no UTF16 Surrogate codepoints" $ decorateList fp validateCharNotUtf16SurrogateCodePoint,
        declare "The path can be identically parsed as an absolute file path." $
          parseAbsFile fp == Just p
      ]

-- | A relative path to a file is valid if:
--
-- * Its path is a relative path
-- * Its path does not have a trailing path separator
-- * Its path is valid according to 'System.FilePath's definition.
-- * Its path is not '.'
-- * Its path is not empty
-- * Its path does not end in '/.'
-- * Its path is not '.'
-- * Its path does not contain '..'.
-- * The path contains no UTF16 Surrogate codepoints
-- * Parsing the path and rendering it again results in the same path.
instance Validity (Path Rel File) where
  validate p@(Path fp) =
    mconcat
      [ declare "The path is relative." $ FilePath.isRelative fp,
        declare "The path has no trailing path separator." $
          not (FilePath.hasTrailingPathSeparator fp),
        declare "System.FilePath considers the path valid." $
          FilePath.isValid fp,
        declare "The path does not equal \".\"" $ fp /= ".",
        declare "The path is not empty" $ not (null fp),
        declare "The path does not end in /." $ not ("/." `isSuffixOf` fp),
        declare "The path does not contain '..'." $ not (".." `isInfixOf` fp),
        decorate "The path contains no UTF16 Surrogate codepoints" $ decorateList fp validateCharNotUtf16SurrogateCodePoint,
        declare "The path can be identically parsed as a relative file path." $
          parseRelFile fp == Just p
      ]

-- | An absolute path to a directory is valid if:
--
-- * Its path is an absolute path
-- * Its path has a trailing path separator
-- * Its path is valid according to 'System.FilePath's definition.
-- * Its path does not contain '..'.
-- * The path contains no UTF16 Surrogate codepoints
-- * Parsing the path and rendering it again results in the same path.
instance Validity (Path Abs Dir) where
  validate p@(Path fp) =
    mconcat
      [ declare "The path is absolute." $ FilePath.isAbsolute fp,
        declare "The path has a trailing path separator." $
          FilePath.hasTrailingPathSeparator fp,
        declare "System.FilePath considers the path valid." $
          FilePath.isValid fp,
        declare "The path does not contain '..'." $ not (".." `isInfixOf` fp),
        decorate "The path contains no UTF16 Surrogate codepoints" $ decorateList fp validateCharNotUtf16SurrogateCodePoint,
        declare "The path can be identically parsed as an absolute directory path." $
          parseAbsDir fp == Just p
      ]

-- | A relative path to a directory is valid if:
--
-- * Its path is a relative path
-- * Its path has a trailing path separator
-- * Its path is valid according to 'System.FilePath's definition.
-- * Its path does not contain '..'.
-- * The path contains no UTF16 Surrogate codepoints
-- * Parsing the path and rendering it again results in the same path.
instance Validity (Path Rel Dir) where
  validate p@(Path fp) =
    mconcat
      [ declare "The path is relative." $ FilePath.isRelative fp,
        declare "The path has a trailing path separator." $
          FilePath.hasTrailingPathSeparator fp,
        declare "System.FilePath considers the path valid." $
          FilePath.isValid fp,
        declare "The path is not empty." $ not (null fp),
        declare "The path does not contain '..'." $ not (".." `isInfixOf` fp),
        decorate "The path contains no UTF16 Surrogate codepoints" $ decorateList fp validateCharNotUtf16SurrogateCodePoint,
        declare "The path can be identically parsed as a relative directory path." $
          parseRelDir fp == Just p
      ]
