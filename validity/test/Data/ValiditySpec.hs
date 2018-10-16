{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}

module Data.ValiditySpec
    ( spec
    ) where

import GHC.Generics (Generic)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif
import Data.Validity
import GHC.Real (Ratio(..))

import Test.Hspec

data Wrong
    = Wrong
    | Fine
    deriving (Show, Eq)

instance Validity Wrong where
    validate w =
        case w of
            Wrong -> invalid "Wrong"
            Fine -> valid

data GeneratedValidity =
    G Rational
      Rational
    deriving (Show, Eq, Generic)

instance Validity GeneratedValidity

spec :: Spec
spec = do
    describe "Wrong" $ do
        it "says Wrong is invalid" $ Wrong `shouldSatisfy` (not . isValid)
        it "says Fine is valid" $ Fine `shouldSatisfy` isValid
    describe "GeneratedValidity" $ do
        let nan = 1 :% 0
        it "says G (1:%0) 0 is not valid" $
            G nan 0 `shouldSatisfy` (not . isValid)
        it "says G 0 (1:%0) is not valid" $
            G 0 nan `shouldSatisfy` (not . isValid)
        it "says G 0 0 is valid" $ G 0 0 `shouldSatisfy` isValid
