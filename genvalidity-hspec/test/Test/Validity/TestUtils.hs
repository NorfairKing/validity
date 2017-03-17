{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}

-- | Utilities to define your own validity-based 'Property's
--
-- You will need @TypeApplications@ to use these.
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
                  let succesful = summaryExamples r > 0 && summaryFailures r > 0
                  pure $ produceResult succesful
        }
#if MIN_VERSION_hspec_core(2,4,0)
produceResult succesful =
    Right $
    if succesful
        then Success
        else Failure Nothing $ Reason "Should have failed but didn't."
#else
produceResult succesful =
    if succesful
        then Success
        else Fail Nothing "Should have failed but didn't."
#endif
shouldFail :: Property -> Property
shouldFail =
    mapResult $ \res ->
        res
        { reason = unwords ["Should have failed:", reason res]
        , expect = not $ expect res
        }
