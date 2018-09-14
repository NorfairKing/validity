{-# LANGUAGE TypeApplications #-}

-- | Standard 'Spec's for 'Show' and 'Read' instances.
--
-- You will need @TypeApplications@ to use these.
module Test.Validity.ShowSpec where

import Test.Hspec

import Data.GenValidity
import Test.Validity.Show
import Test.Validity.Utils

spec :: Spec
spec = do
    showReadSpecOnValid @Double
    showReadSpec @Int
    showReadSpecOnArbitrary @Double
    showReadSpecOnGen ((* 2) <$> genValid @Int) "even" (const [])
    failsBecause "show and read don't have the correct semantics" $
        showReadSpec @ShowFuncMismatch

data ShowFuncMismatch =
    ShowFuncMismatch
    deriving (Eq, Read)

instance Show ShowFuncMismatch where
    show ShowFuncMismatch = "wrong"

instance GenUnchecked ShowFuncMismatch where
    genUnchecked = pure ShowFuncMismatch
    shrinkUnchecked _ = []
