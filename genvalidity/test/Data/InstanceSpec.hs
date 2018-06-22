{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Data.InstanceSpec
    ( spec
    ) where

import Data.Data
import Data.Int
#if MIN_VERSION_base(4,9,0)
import Data.List.NonEmpty (NonEmpty)
#endif
import Data.Fixed
import Data.Ratio
import Data.Word

import Control.Monad

import Test.Hspec
import Test.QuickCheck
#if MIN_VERSION_base(4,8,0)
import GHC.Natural
#else
import Control.Applicative (pure)
#endif
import Data.GenValidity


spec :: Spec
spec = do
    twoTests (Proxy :: Proxy ())
    twoTests (Proxy :: Proxy Bool)
    twoTests (Proxy :: Proxy Ordering)
    twoTests (Proxy :: Proxy Char)
    twoTests (Proxy :: Proxy Word)
    twoTests (Proxy :: Proxy Word8)
    twoTests (Proxy :: Proxy Word16)
    twoTests (Proxy :: Proxy Word32)
    twoTests (Proxy :: Proxy Word64)
    twoTests (Proxy :: Proxy Int)
    twoTests (Proxy :: Proxy Int8)
    twoTests (Proxy :: Proxy Int16)
    twoTests (Proxy :: Proxy Int32)
    twoTests (Proxy :: Proxy Int64)
    twoTests (Proxy :: Proxy Integer)
    threeTests (Proxy :: Proxy Float)
    threeTests (Proxy :: Proxy Double)
    threeTests (Proxy :: Proxy (Double, Double))
    threeTests (Proxy :: Proxy (Double, Double, Double))
    threeTests (Proxy :: Proxy (Double, Double, Double, Double))
    threeTests (Proxy :: Proxy (Double, Double, Double, Double, Double))
    threeTests (Proxy :: Proxy (Either Double Double))
    threeTests (Proxy :: Proxy (Maybe Double))
    threeTests (Proxy :: Proxy (Maybe (Maybe Double)))
    threeTests (Proxy :: Proxy [Double])
    threeTests (Proxy :: Proxy (Ratio Integer))
    threeTests (Proxy :: Proxy (Ratio Int))
    twoTests (Proxy :: Proxy Uni)
    twoTests (Proxy :: Proxy Deci)
    twoTests (Proxy :: Proxy Centi)
    twoTests (Proxy :: Proxy Milli)
    twoTests (Proxy :: Proxy Micro)
    twoTests (Proxy :: Proxy Nano)
    twoTests (Proxy :: Proxy Pico)
#if MIN_VERSION_base(4,8,0)
    twoTests (Proxy :: Proxy Natural)
#endif
#if MIN_VERSION_base(4,9,0)
    threeTests (Proxy :: Proxy (NonEmpty Double))
#endif
twoTests ::
       forall a. (Show a, Typeable a, GenValid a)
    => Proxy a
    -> Spec
twoTests proxy =
    describe (nameOf proxy) $ do
        genUncheckedTest proxy
        genValidTest proxy

threeTests ::
       forall a. (Show a, Typeable a, GenValid a, GenInvalid a)
    => Proxy a
    -> Spec
threeTests proxy =
    describe (nameOf proxy) $ do
        genUncheckedTest proxy
        genValidTest proxy
        genInvalidTest proxy

genUncheckedTest ::
       forall a. (Show a, Typeable a, GenValid a)
    => Proxy a
    -> Spec
genUncheckedTest proxy = do
    it (unwords
            ["genUnchecked of", nameOf proxy, "does not crash while validating"]) $
        forAll genUnchecked $ \a ->
            case prettyValidation (a :: a) of
                Right v -> seq v True
                Left err -> seq err True
    it (unwords
            [ "shrinkUnchecked of"
            , nameOf proxy
            , "only produces values that do not crash while validating"
            ]) $
        forAll genUnchecked $ \a ->
            forM_ (shrinkUnchecked a) $ \v ->
                case prettyValidation (v :: a) of
                    Right v_ -> seq v_ $ pure () :: IO ()
                    Left err -> seq err $ pure ()

genValidTest ::
       forall a. (Show a, Typeable a, GenValid a)
    => Proxy a
    -> Spec
genValidTest proxy = do
    it (unwords ["genValid of", nameOf proxy, "generates only valid values"]) $
        forAll genValid $ \a ->
            case prettyValidation (a :: a) of
                Right v -> seq v $ pure ()
                Left err ->
                    expectationFailure $
                    unlines
                        [ "'validate' reported this value to be invalid:"
                        , show a
                        , err
                        , ""
                        ]
    it (unwords ["shrinkValid of", nameOf proxy, "shrinks to only valid values"]) $
        forAll genValid $ \a ->
            forM_ (shrinkValid a) $ \v ->
                case prettyValidation (v :: a) of
                    Right v_ -> seq v_ $ pure ()
                    Left err ->
                        expectationFailure $
                        unlines
                            [ "'validate' reported this value to be invalid:"
                            , show v
                            , err
                            , ""
                            ]
    it (unwords
            [ "shrinkValid of"
            , nameOf proxy
            , "only produces values that do not crash while validating"
            ]) $
        forAll genValid $ \a ->
            forM_ (shrinkValid a) $ \v ->
                case prettyValidation (v :: a) of
                    Right v_ -> seq v_ $ pure () :: IO ()
                    Left err -> seq err $ pure ()

genInvalidTest ::
       forall a. (Show a, Typeable a, GenInvalid a)
    => Proxy a
    -> Spec
genInvalidTest proxy = do
    it (unwords ["genInvalid of", nameOf proxy, "generates only invalid values"]) $
        forAll genInvalid $ \a ->
            case prettyValidation (a :: a) of
                Right _ ->
                    expectationFailure $
                    unlines
                        ["'validate' reported this value to be valid: ", show a]
                Left e -> seq e $ pure ()
    it (unwords
            ["shrinkInvalid of", nameOf proxy, "shrinks to only invalid values"]) $
        forAll genInvalid $ \a ->
            forM_ (shrinkInvalid a) $ \v ->
                case prettyValidation (v :: a) of
                    Right _ ->
                        expectationFailure $
                        unlines
                            [ "'validate' reported this value to be valid: "
                            , show v
                            ]
                    Left e -> seq e $ pure ()
    it (unwords
            [ "shrinkInvalid of"
            , nameOf proxy
            , "only produces values that do not crash while validating"
            ]) $
        forAll genInvalid $ \a ->
            forM_ (shrinkInvalid a) $ \v ->
                case prettyValidation (v :: a) of
                    Right _ ->
                        expectationFailure $
                        unlines
                            [ "'validate' reported this value to be valid: "
                            , show v
                            ]
                    Left e -> seq e $ pure ()

nameOf ::
       forall a. Typeable a
    => Proxy a
    -> String
nameOf = show . typeRep
