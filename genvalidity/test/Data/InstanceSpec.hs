{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Data.InstanceSpec
    ( spec
    ) where
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>), pure)
import Data.Functor ((<$>))
#endif
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
import Test.Hspec.Core.QuickCheck (modifyMaxSize, modifyMaxSuccess)
import Test.QuickCheck
#if MIN_VERSION_base(4,8,0)
import GHC.Natural
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
    twoTests (Proxy :: Proxy Float)
    twoTupleTests (Proxy :: Proxy Float)
    twoTests (Proxy :: Proxy Double)
    twoTupleTests (Proxy :: Proxy Double)
    threeTests (Proxy :: Proxy Rational)
    threeTupleTests (Proxy :: Proxy Rational)
    modifyMaxSize (`quot` 2) $
        threeTests (Proxy :: Proxy (Either Rational Rational))
    threeTests (Proxy :: Proxy (Maybe Rational))
    threeTests (Proxy :: Proxy (Maybe (Maybe Rational)))
    threeTests (Proxy :: Proxy [Rational])
    threeTests (Proxy :: Proxy (Ratio Integer))
    threeTests (Proxy :: Proxy (Ratio Integer))
    threeTupleTests (Proxy :: Proxy (Ratio Integer))
    threeTests (Proxy :: Proxy (Ratio Int))
    threeTupleTests (Proxy :: Proxy (Ratio Int))
    twoTests (Proxy :: Proxy Uni)
    twoTupleTests (Proxy :: Proxy Uni)
    twoTests (Proxy :: Proxy Deci)
    twoTupleTests (Proxy :: Proxy Deci)
    twoTests (Proxy :: Proxy Centi)
    twoTupleTests (Proxy :: Proxy Centi)
    twoTests (Proxy :: Proxy Milli)
    twoTupleTests (Proxy :: Proxy Milli)
    twoTests (Proxy :: Proxy Micro)
    twoTupleTests (Proxy :: Proxy Micro)
    twoTests (Proxy :: Proxy Nano)
    twoTupleTests (Proxy :: Proxy Nano)
    twoTests (Proxy :: Proxy Pico)
    twoTupleTests (Proxy :: Proxy Pico)
#if MIN_VERSION_base(4,8,0)
    twoTests (Proxy :: Proxy Natural)

    twoTupleTests (Proxy :: Proxy Natural)

    twoTests (Proxy :: Proxy (Ratio Integer))

    twoTupleTests (Proxy :: Proxy (Ratio Int))
#endif
#if MIN_VERSION_base(4,9,0)
    threeTests (Proxy :: Proxy (NonEmpty Rational))
#endif
twoTupleTests ::
       forall a. (Show a, Eq a, Typeable a, GenValid a)
    => Proxy a
    -> Spec
twoTupleTests proxy = do
    modifyMaxSize (`quot` 2) $ twoTests $ (,) <$> proxy <*> proxy
    modifyMaxSize (`quot` 3) $ twoTests $ (,,) <$> proxy <*> proxy <*> proxy
    modifyMaxSize (`quot` 4) $
        twoTests $ (,,,) <$> proxy <*> proxy <*> proxy <*> proxy
    modifyMaxSize (`quot` 5) $
        twoTests $ (,,,,) <$> proxy <*> proxy <*> proxy <*> proxy <*> proxy

threeTupleTests ::
       forall a. (Show a, Eq a, Typeable a, GenValid a, GenInvalid a)
    => Proxy a
    -> Spec
threeTupleTests proxy = do
    modifyMaxSize (`quot` 2) $ threeTests $ (,) <$> proxy <*> proxy
    modifyMaxSize (`quot` 3) $ threeTests $ (,,) <$> proxy <*> proxy <*> proxy
    modifyMaxSize (`quot` 4) $
        threeTests $ (,,,) <$> proxy <*> proxy <*> proxy <*> proxy
    modifyMaxSize (`quot` 5) $
        threeTests $ (,,,,) <$> proxy <*> proxy <*> proxy <*> proxy <*> proxy

twoTests ::
       forall a. (Show a, Eq a, Typeable a, GenValid a)
    => Proxy a
    -> Spec
twoTests proxy =
    describe (nameOf proxy) $ do
        genUncheckedTest proxy
        genValidTest proxy

threeTests ::
       forall a. (Show a, Eq a, Typeable a, GenValid a, GenInvalid a)
    => Proxy a
    -> Spec
threeTests proxy =
    describe (nameOf proxy) $ do
        genUncheckedTest proxy
        genValidTest proxy
        genInvalidTest proxy

genUncheckedTest ::
       forall a. (Show a, Eq a, Typeable a, GenValid a)
    => Proxy a
    -> Spec
genUncheckedTest proxy = do
    it (unwords
            ["genUnchecked of", nameOf proxy, "does not crash while validating"]) $
        forAll genUnchecked $ \a ->
            case prettyValidate (a :: a) of
                Right v -> seq v True
                Left err -> seq err True
    modifyMaxSuccess (`quot` 5) $
        it
            (unwords
                 [ "shrinkUnchecked of"
                 , nameOf proxy
                 , "only produces values that do not crash while validating"
                 ]) $
        forAll genUnchecked $ \a ->
            forM_ (shrinkUnchecked a) $ \v ->
                case prettyValidate (v :: a) of
                    Right v_ -> seq v_ $ pure () :: IO ()
                    Left err -> seq err $ pure ()
    modifyMaxSuccess (`quot` 5) $
        it
            (unwords
                 [ "shrinkUnchecked of"
                 , nameOf proxy
                 , "does not shrink to itself"
                 ]) $
        forAll genValid $ \a ->
            forM_ (shrinkUnchecked a) $ \a' ->
                unless (a /= a') $
                expectationFailure $
                unlines ["The value", show (a :: a), "was shrunk to itself"]

genValidTest ::
       forall a. (Show a, Eq a, Typeable a, GenValid a)
    => Proxy a
    -> Spec
genValidTest proxy = do
    it (unwords ["genValid of", nameOf proxy, "generates only valid values"]) $
        forAll genValid $ \a ->
            case prettyValidate (a :: a) of
                Right v -> seq v $ pure ()
                Left err ->
                    expectationFailure $
                    unlines
                        [ "'validate' reported this value to be invalid:"
                        , show a
                        , err
                        , ""
                        ]
    modifyMaxSuccess (`quot` 5) $
        it
            (unwords
                 [ "shrinkValid of"
                 , nameOf proxy
                 , "shrinks to only valid values"
                 ]) $
        forAll genValid $ \a ->
            forM_ (shrinkValid a) $ \v ->
                case prettyValidate (v :: a) of
                    Right v_ -> seq v_ $ pure ()
                    Left err ->
                        expectationFailure $
                        unlines
                            [ "'validate' reported this value to be invalid:"
                            , show v
                            , err
                            , ""
                            ]
    modifyMaxSuccess (`quot` 5) $
        it
            (unwords
                 [ "shrinkValid of"
                 , nameOf proxy
                 , "only produces values that do not crash while validating"
                 ]) $
        forAll genValid $ \a ->
            forM_ (shrinkValid a) $ \v ->
                case prettyValidate (v :: a) of
                    Right v_ -> seq v_ $ pure () :: IO ()
                    Left err -> seq err $ pure ()
    modifyMaxSuccess (`quot` 5) $
        it
            (unwords
                 ["shrinkValid of", nameOf proxy, "does not shrink to itself"]) $
        forAll genValid $ \a ->
            forM_ (shrinkValid a) $ \a' ->
                unless (a /= a') $
                expectationFailure $
                unlines ["The value", show (a :: a), "was shrunk to itself"]

genInvalidTest ::
       forall a. (Show a, Typeable a, GenInvalid a)
    => Proxy a
    -> Spec
genInvalidTest proxy = do
    it (unwords ["genInvalid of", nameOf proxy, "generates only invalid values"]) $
        forAll genInvalid $ \a ->
            case prettyValidate (a :: a) of
                Right _ ->
                    expectationFailure $
                    unlines
                        ["'validate' reported this value to be valid: ", show a]
                Left e -> seq e $ pure ()
    modifyMaxSuccess (`quot` 5) $
        it
            (unwords
                 [ "shrinkInvalid of"
                 , nameOf proxy
                 , "shrinks to only invalid values"
                 ]) $
        forAll genInvalid $ \a ->
            forM_ (shrinkInvalid a) $ \v ->
                case prettyValidate (v :: a) of
                    Right _ ->
                        expectationFailure $
                        unlines
                            [ "'validate' reported this value to be valid: "
                            , show v
                            ]
                    Left e -> seq e $ pure ()
    modifyMaxSuccess (`quot` 5) $
        it
            (unwords
                 [ "shrinkInvalid of"
                 , nameOf proxy
                 , "only produces values that do not crash while validating"
                 ]) $
        forAll genInvalid $ \a ->
            forM_ (shrinkInvalid a) $ \v ->
                case prettyValidate (v :: a) of
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
