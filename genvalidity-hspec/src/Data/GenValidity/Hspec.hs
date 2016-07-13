{-|

 These are standard tests that you should add to your @hspec@ test suite
 if you implemented @GenValidity@ instances for your own data types.

 Use them like this:

 > mySpec :: Spec
 > mySpec = do
 >     genspec (proxy :: MyType)
 >     genspec (proxy :: MyOtherType)

 HSpec will take care of the rest.

 -}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    let name = show $ typeOf proxy
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
    let name = show $ typeOf proxy
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
    let name = show $ typeOf proxy
    describe "genValid" $ do
        it ("only generates valid \'" ++ name ++ "\'s") $ do
            forAll genValid $ \a ->
                (a `asTypeOf` proxy) `shouldSatisfy` isValid

    describe "genInvalid" $ do
        it ("only generates invalid \'" ++ name ++ "\'s") $ do
            forAll genInvalid $ \a ->
                (a `asTypeOf` proxy) `shouldNotSatisfy` isValid


class CanFail f where
    hasFailed :: f a -> Bool
    resultIfSucceeded :: f a -> Maybe a

instance CanFail Maybe where
    hasFailed Nothing = True
    hasFailed _ = False

    resultIfSucceeded Nothing = Nothing
    resultIfSucceeded (Just r) = Just r

instance CanFail (Either e) where
    hasFailed (Left _) = True
    hasFailed _ = False

    resultIfSucceeded (Left _) = Nothing
    resultIfSucceeded (Right r) = Just r

producesValidsOnGen
    :: (Show a, Show b, Validity b)
    => (a -> b)
    -> Gen a
    -> Property
producesValidsOnGen func gen
    = forAll gen $ \a -> func a `shouldSatisfy` isValid

alwaysProducesValid
    :: (Show a, Show b, GenValidity a, Validity b)
    => (a -> b)
    -> Property
alwaysProducesValid = (`producesValidsOnGen` genUnchecked)

producesValidsOnValids
    :: (Show a, Show b, GenValidity a, Validity b)
    => (a -> b)
    -> Property
producesValidsOnValids = (`producesValidsOnGen` genValid)

producesValidsOnGens2
    :: (Show a, Show b, Show c, Validity c)
    => (a -> b -> c)
    -> Gen a -> Gen b
    -> Property
producesValidsOnGens2 func gen1 gen2
    = forAll gen1 $ \a ->
          forAll gen2 $ \b ->
              func a b `shouldSatisfy` isValid

alwaysProducesValid2
    :: (Show a, Show b, Show c, GenValidity a, GenValidity b, Validity c)
    => (a -> b -> c)
    -> Property
alwaysProducesValid2 func
    = producesValidsOnGens2 func genUnchecked genUnchecked

producesValidsOnValids2
    :: (Show a, Show b, Show c, GenValidity a, GenValidity b, Validity c)
    => (a -> b -> c)
    -> Property
producesValidsOnValids2 func
    = producesValidsOnGens2 func genValid genValid

succeedsOnGen
    :: (Show a, Show b, Show (f b), CanFail f)
    => (a -> f b)
    -> Gen a
    -> Property
succeedsOnGen func gen
    = forAll gen $ \a -> func a `shouldNotSatisfy` hasFailed

succeedsOnValidInput
    :: (Show a, Show b, Show (f b), GenValidity a, CanFail f)
    => (a -> f b)
    -> Spec
succeedsOnValidInput func =
    it "succeeds if the input is valid" $
        func `succeedsOnGen` genValid

failsOnGen
    :: (Show a, Show b, Show (f b), CanFail f)
    => (a -> f b)
    -> Gen a
    -> Property
failsOnGen func gen
    = forAll gen $ \a -> func a `shouldSatisfy` hasFailed

failsOnInvalidInput
    :: (Show a, Show b, Show (f b), GenValidity a, CanFail f)
    => (a -> f b)
    -> Spec
failsOnInvalidInput func
    = it "fails if the input is invalid" $ do
        func `failsOnGen` genInvalid

validIfSucceedsOnGen
    :: (Show a, Show b, Show (f b), Validity b, CanFail f)
    => (a -> f b)
    -> Gen a
    -> Property
validIfSucceedsOnGen func gen
    = forAll gen $ \a ->
        case resultIfSucceeded (func a) of
            Nothing  -> return () -- Can happen
            Just res -> res `shouldSatisfy` isValid

validIfSucceeds
  :: (Show a, Show b, Show (f b), GenValidity a, Validity b, CanFail f)
  => (a -> f b)
  -> Spec
validIfSucceeds func
    = it "produces valid output if it succeeds" $ do
        func `validIfSucceedsOnGen` genUnchecked
