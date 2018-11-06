{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.GenValidity.GenericSpec
    ( spec
    ) where

import GHC.Generics (Generic, Rep)

import Control.Monad

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
    describe "shrinkValidStructurally" $ do
        shrinkValidstructurallySpec (Proxy :: Proxy Bool)
        shrinkValidstructurallySpec (Proxy :: Proxy Ordering)
        shrinkValidstructurallySpec (Proxy :: Proxy (Maybe Double))
        shrinkValidstructurallySpec (Proxy :: Proxy (Either Double Rational))
        shrinkValidstructurallySpec (Proxy :: Proxy MyType)

genValidstructurallySpec ::
       forall a.
       (Validity a, Show a, Eq a, Typeable a, Generic a, GGenValid (Rep a))
    => Proxy a
    -> Spec
genValidstructurallySpec proxy =
    it (unwords ["only generates valid", "\"" ++ nameOf proxy ++ "\"s"]) $
    forAll (genValidStructurally :: Gen a) $ \a ->
        case prettyValidate a of
            Right _ -> return ()
            Left err ->
                expectationFailure $
                unlines
                    [ "'validate' reported this value to be invalid: "
                    , show a
                    , "with explanation"
                    , err
                    , ""
                    ]

shrinkValidstructurallySpec ::
       forall a.
       ( Validity a
       , Show a
       , Eq a
       , Typeable a
       , Generic a
       , GenValid a
       , GValidRecursivelyShrink (Rep a)
       , GValidSubterms (Rep a) a
       )
    => Proxy a
    -> Spec
shrinkValidstructurallySpec proxy = do
    it (unwords ["only shrinks to valid", "\"" ++ nameOf proxy ++ "\"s"]) $
        forAll (genValid :: Gen a) $ \a ->
            forM_ (shrinkValidStructurally a) $ \subA ->
                case prettyValidate subA of
                    Right _ -> return ()
                    Left err ->
                        expectationFailure $
                        unlines
                            [ "'validate' reported this value to be invalid: "
                            , show subA
                            , "with explanation"
                            , err
                            , "but it should have been valid from shrinking"
                            ]
    it (unwords
            ["never shrinks to itself for valid", "\"" ++ nameOf proxy ++ "\"s"]) $
        forAll (genValid :: Gen a) $ \a ->
            forM_ (shrinkValidStructurally a) $ \subA ->
                when (subA == a) $
                expectationFailure $ unlines [show a, "was shrunk to itself."]

nameOf ::
       forall a. Typeable a
    => Proxy a
    -> String
nameOf = show . typeRep

data MyType =
    MyType Double
           Rational
    deriving (Show, Eq, Generic, Typeable)

instance Validity MyType

instance GenUnchecked MyType

instance GenValid MyType
