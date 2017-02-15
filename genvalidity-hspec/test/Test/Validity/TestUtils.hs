{-# LANGUAGE TypeApplications #-}

module Test.Validity.TestUtils where

import Test.Hspec

import Data.GenValidity
import Test.Validity

import Test.Hspec.Core.Formatters
import Test.Hspec.Core.Runner
import Test.Hspec.Core.Spec
import Test.QuickCheck.Property

failsBecause :: String -> SpecWith () -> SpecWith ()
failsBecause s st = mapSpecTree go st
  where
    go :: SpecTree () -> SpecTree ()
    go sp =
        Leaf $
        Item
        { itemRequirement = s
        , itemLocation = Nothing
        , itemIsParallelizable = False
        , itemExample =
              \ps runner callback -> do
                  let conf = defaultConfig {configFormatter = Just silent}
                  r <- hspecWithResult conf $ fromSpecList [sp]
                  pure $
                      if summaryExamples r > 0 && summaryFailures r > 0
                          then Success
                          else Fail Nothing "Should have failed but didn't."
        }

shouldFail :: Property -> Property
shouldFail =
    mapResult $ \res ->
        res
        { reason = unwords ["Should have failed:", reason res]
        , expect = not $ expect res
        }
