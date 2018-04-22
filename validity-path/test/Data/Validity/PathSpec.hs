{-# LANGUAGE DeriveGeneric #-}

module Data.Validity.PathSpec
    ( spec
    ) where

import Test.Hspec
import Test.Validity

import qualified System.FilePath

import Control.Monad

import Data.Validity.Path ()

import Path
import Path.Internal

spec :: Spec
spec = do
    describe "Path Abs File" $ do
        let p :: String -> Path Abs File
            p = Path
        it "negatively checks for absolute paths" $ shouldBeInvalid $ p "./test"
        it "positively checks for absolute paths" $ shouldBeValid $ p "/test"
        it "checks for absolute paths" $
            forAllValid $ \path ->
                when (not (System.FilePath.isAbsolute path)) $
                shouldBeInvalid $ p path
        it "negatively checks for trailing path separators" $
            shouldBeInvalid $ p "/test/"
        it "positively checks for trailing path separators" $
            shouldBeValid $ p "/test"
        it "negatively checks for being \".\"" $ shouldBeInvalid $ p "."
        it "negatively checks for ending in \"/.\"" $
            shouldBeInvalid $ p "/test/."
        it "negatively checks for containing \"..\"" $
            shouldBeInvalid $ p "/test/../file"
        it "checks for isValid from System.FilePath" $
            forAllValid $ \path ->
                when (not (System.FilePath.isValid path)) $
                shouldBeInvalid $ p path
    describe "Path Rel File" $ do
        let p :: String -> Path Rel File
            p = Path
        it "checks for relative paths" $
            forAllValid $ \path ->
                when (not (System.FilePath.isRelative path)) $
                shouldBeInvalid $ p path
        it "checks that the path is not empty" $ shouldBeInvalid $ p ""
        it "negatively checks for being \".\"" $ shouldBeInvalid $ p "."
        it "negatively checks for ending in \"/.\"" $
            shouldBeInvalid $ p "test/."
        it "negatively checks for containing \"..\"" $
            shouldBeInvalid $ p "/test/../file"
        it "checks for isValid from System.FilePath" $
            forAllValid $ \path ->
                when (not (System.FilePath.isValid path)) $
                shouldBeInvalid $ p path
    describe "Path Abs Dir" $ do
        let p :: String -> Path Abs Dir
            p = Path
        it "checks for absolute paths" $
            forAllValid $ \path ->
                when (not (System.FilePath.isAbsolute path)) $
                shouldBeInvalid $ p path
        it "negatively checks for containing \"..\"" $
            shouldBeInvalid $ p "/test/../dir"
        it "checks for isValid from System.FilePath" $
            forAllValid $ \path ->
                when (not (System.FilePath.isValid path)) $
                shouldBeInvalid $ p path
    describe "Path Rel Dir" $ do
        let p :: String -> Path Rel Dir
            p = Path
        it "checks for relative paths" $
            forAllValid $ \path ->
                when (not (System.FilePath.isRelative path)) $
                shouldBeInvalid $ p path
        it "checks that the path is not empty" $ shouldBeInvalid $ p ""
        it "negatively checks for containing \"..\"" $
            shouldBeInvalid $ p "test/../dir"
        it "checks for isValid from System.FilePath" $
            forAllValid $ \path ->
                when (not (System.FilePath.isValid path)) $
                shouldBeInvalid $ p path
