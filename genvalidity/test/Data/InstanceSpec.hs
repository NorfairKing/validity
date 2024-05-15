{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.InstanceSpec
  ( spec,
  )
where

import Control.Monad
import Data.Data
import Data.Fixed
import Data.GenValidity
import Data.Int
import Data.List.NonEmpty (NonEmpty)
import Data.Ratio
import Data.Word
import Numeric.Natural
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSize, modifyMaxSuccess)
import Test.QuickCheck

spec :: Spec
spec = do
  genValidTest (Proxy :: Proxy ())
  genValidTest (Proxy :: Proxy Bool)
  genValidTest (Proxy :: Proxy Ordering)
  genValidTest (Proxy :: Proxy Char)
  genValidTest (Proxy :: Proxy Word)
  genValidTest (Proxy :: Proxy Word8)
  genValidTest (Proxy :: Proxy Word16)
  genValidTest (Proxy :: Proxy Word32)
  genValidTest (Proxy :: Proxy Word64)
  genValidTest (Proxy :: Proxy Int)
  genValidTest (Proxy :: Proxy Int8)
  genValidTest (Proxy :: Proxy Int16)
  genValidTest (Proxy :: Proxy Int32)
  genValidTest (Proxy :: Proxy Int64)
  genValidTest (Proxy :: Proxy Integer)
  genValidTest (Proxy :: Proxy Float)
  tupleTest (Proxy :: Proxy Float)
  -- Regression tests
  describe "shrinkValid Float" $ do
    let sf :: Float -> Spec
        sf f = it (unwords ["Does not shrink", show f, "to itself"]) $ f `shouldNotSatisfy` (`elem` shrinkValid f)

    sf (-2.1393704e20)
    sf 1.2223988e-12
    sf 2.7896812e10
  describe "shrinkValid Double" $ do
    let sd :: Double -> Spec
        sd d = it (unwords ["Does not shrink", show d, "to itself"]) $ d `shouldNotSatisfy` (`elem` shrinkValid d)
    sd (-1.032730679986007e18)
  genValidTest (Proxy :: Proxy Double)
  tupleTest (Proxy :: Proxy Double)
  genValidTest (Proxy :: Proxy (Ratio Int))
  modifyMaxSuccess (`quot` 2) $
    modifyMaxSize (`quot` 2) $
      genValidTest (Proxy :: Proxy (Either Bool Ordering))
  genValidTest (Proxy :: Proxy (Maybe Ordering))
  genValidTest (Proxy :: Proxy (Maybe (Maybe (Ordering))))
  genValidTest (Proxy :: Proxy (Ratio Integer))
  -- threeTupleTests (Proxy :: Proxy (Ratio Integer))
  genValidTest (Proxy :: Proxy (Ratio Int))
  -- threeTupleTests (Proxy :: Proxy (Ratio Int))
  genValidTest (Proxy :: Proxy (Ratio Int8))
  describe "shrinking (Ratio Int)" $
    it "can shrink this example" $
      let v = ((-9223372036854775808) % 9223372036854775761) :: Ratio Int
       in v `notElem` shrinkValid v
  describe "shrinking (Ratio Int8)" $
    it "can shrink this example" $
      let v = ((-128) % 113) :: Ratio Int8
       in v `notElem` shrinkValid v
  genValidTest (Proxy :: Proxy Uni)
  tupleTest (Proxy :: Proxy Uni)
  genValidTest (Proxy :: Proxy Deci)
  tupleTest (Proxy :: Proxy Deci)
  genValidTest (Proxy :: Proxy Centi)
  tupleTest (Proxy :: Proxy Centi)
  genValidTest (Proxy :: Proxy Milli)
  tupleTest (Proxy :: Proxy Milli)
  genValidTest (Proxy :: Proxy Micro)
  tupleTest (Proxy :: Proxy Micro)
  genValidTest (Proxy :: Proxy Nano)
  tupleTest (Proxy :: Proxy Nano)
  genValidTest (Proxy :: Proxy Pico)
  tupleTest (Proxy :: Proxy Pico)
  genValidTest (Proxy :: Proxy Natural)
  tupleTest (Proxy :: Proxy Natural)
  genValidTest (Proxy :: Proxy (NonEmpty Ordering))

tupleTest ::
  forall a.
  (Show a, Eq a, Typeable a, GenValid a) =>
  Proxy a ->
  Spec
tupleTest proxy = do
  modifyMaxSuccess (`quot` 2) $ modifyMaxSize (`quot` 2) $ genValidTest $ (,) <$> proxy <*> proxy

genValidTest ::
  forall a.
  (Show a, Eq a, Typeable a, GenValid a) =>
  Proxy a ->
  Spec
genValidTest proxy = do
  it (unwords ["genValid of", nameOf proxy, "generates only valid values"]) $
    forAll genValid $ \a ->
      case prettyValidate (a :: a) of
        Right v -> seq v $ pure ()
        Left err ->
          expectationFailure $
            unlines ["'validate' reported this value to be invalid:", show a, err, ""]
  modifyMaxSuccess (`quot` 5) $
    it (unwords ["shrinkValid of", nameOf proxy, "shrinks to only valid values"]) $
      forAll genValid $ \a ->
        forM_ (shrinkValid a) $ \v ->
          case prettyValidate (v :: a) of
            Right v_ -> seq v_ $ pure ()
            Left err ->
              expectationFailure $
                unlines ["'validate' reported this value to be invalid:", show v, err, ""]
  modifyMaxSuccess (`quot` 5)
    $ it
      ( unwords
          ["shrinkValid of", nameOf proxy, "only produces values that do not crash while validating"]
      )
    $ forAll genValid
    $ \a ->
      forM_ (shrinkValid a) $ \v ->
        case prettyValidate (v :: a) of
          Right v_ -> seq v_ $ pure () :: IO ()
          Left err -> seq err $ pure ()
  modifyMaxSuccess (`quot` 5) $
    it (unwords ["shrinkValid of", nameOf proxy, "does not shrink to itself"]) $
      forAll genValid $ \a ->
        forM_ (shrinkValid a) $ \a' ->
          unless (a /= a') $
            expectationFailure $
              unlines ["The value", show (a :: a), "was shrunk to itself"]

nameOf ::
  forall a.
  (Typeable a) =>
  Proxy a ->
  String
nameOf = show . typeRep
