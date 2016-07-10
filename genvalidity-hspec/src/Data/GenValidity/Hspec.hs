module Data.GenValidity.Hspec
    ( module Data.GenValidity
    , module Data.GenValidity.Hspec
    ) where

import           Data.GenValidity

import           Test.Hspec
import           Test.QuickCheck

import           Data.Data

-- | A value of arbitrary type, used to specify which type to generate a spec
-- for.
proxy :: a
proxy = undefined

-- | A combination of @arbitrarySpec@ and @validitySpec@
--
-- Example usage:
--
-- > genspec (proxy :: MyData)
genspec
    :: (Show a, Eq a, Data a, GenValidity a, Arbitrary a)
    => a
    -> Spec
genspec proxy = do
    let name = dataTypeName $ dataTypeOf proxy
    describe ("GenSpec for " ++ name) $ do
        arbitrarySpec proxy
        validitySpec proxy

-- | A @Spec@ that specifies that @arbitrary@ only generates data that
-- satisfy @isValid@ and that @shrink@ only produces data that satisfy
-- @isValid@.
--
-- Example usage:
--
-- > arbitrarySpec (proxy :: MyData)
arbitrarySpec
    :: (Typeable a, Show a, Eq a, Data a, GenValidity a, Arbitrary a)
    => a
    -> Spec
arbitrarySpec proxy = do
    let name = dataTypeName $ dataTypeOf proxy
    describe ("Arbitrary " ++ name) $ do
        it ("is instantiated such that 'arbitrary' only generates valid \'"
            ++ name
            ++ "\'s") $ do
            forAll arbitrary $ \a ->
                (a `asTypeOf` proxy) `shouldSatisfy` isValid

        it ("is instantiated such that 'shrink' only produces valid \'"
            ++ name
            ++ "\'s") $ do
            forAll arbitrary $ \a ->
                shrink (a `asTypeOf` proxy) `shouldSatisfy` all isValid

-- | A @Spec@ that specifies that @genValid@ only generates valid data and that
-- @genInvalid@ only generates invalid data.
--
-- In general it is a good idea to add this spec to your test suite if you
-- write a custom implementation of @genValid@ or @genInvalid@.
--
-- Example usage:
--
-- > validitySpec (proxy :: MyData)
validitySpec
    :: (Typeable a, Show a, Eq a, Data a, GenValidity a, Arbitrary a)
    => a
    -> Spec
validitySpec proxy = do
    let name = dataTypeName $ dataTypeOf proxy
    describe "genValid" $ do
        it ("only generates valid \'" ++ name ++ "\'s") $ do
            forAll genValid $ \a ->
                (a `asTypeOf` proxy) `shouldSatisfy` isValid

    describe "genInvalid" $ do
        it ("only generates invalid \'" ++ name ++ "\'s") $ do
            forAll genInvalid $ \a ->
                (a `asTypeOf` proxy) `shouldNotSatisfy` isValid


