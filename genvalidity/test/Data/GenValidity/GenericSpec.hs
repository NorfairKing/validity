{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.GenValidity.GenericSpec
    ( spec
    ) where

import GHC.Generics (Generic, Rep)

import Test.Hspec
import Test.QuickCheck

import Data.Proxy
import Data.Typeable

import Data.GenValidity

spec :: Spec
spec = do
    describe "genValidStructurally" $ do
        genValidstructurallySpec (Proxy :: Proxy Bool)
        genValidstructurallySpec (Proxy :: Proxy Ordering)
        genValidstructurallySpec (Proxy :: Proxy (Maybe Double))
        genValidstructurallySpec (Proxy :: Proxy (Either Double Rational))
        genValidstructurallySpec (Proxy :: Proxy MyType)

genValidstructurallySpec ::
       forall a.
       (Validity a, Show a, Eq a, Typeable a, Generic a, GGenValid (Rep a))
    => Proxy a
    -> Spec
genValidstructurallySpec proxy =
    it (unwords ["only generates valid", nameOf proxy]) $
    forAll (genValidStructurally :: Gen a) $ \a ->
        case prettyValidation a of
            Right _ -> pure ()
            Left err ->
                expectationFailure $
                unlines
                    [ "'validate' reported this value to be invalid: "
                    , show a
                    , "with explanation"
                    , err
                    , ""
                    ]

nameOf ::
       forall a. Typeable a
    => Proxy a
    -> String
nameOf = show . typeRep

data MyType =
    MyType Double
           Rational
    deriving (Show, Eq, Generic)

instance Validity MyType
