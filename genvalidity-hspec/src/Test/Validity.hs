{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity
    ( module Data.GenValidity
    , Proxy(Proxy)

    -- * Tests for Validity instances
    , validitySpec
    , validityLaws
    , validityLawsForGen

    -- * Tests for Arbitrary instances involving Validity
    , arbitrarySpec
    , arbitraryGeneratesOnlyValid
    , shrinkProducesOnlyValids

    -- * Tests for GenValidity instances
    , genValiditySpec
    , genValidityValidGeneratesValid
    , genGeneratesValid
    , genValidityInvalidGeneratesInvalid
    , genGeneratesInvalid

    -- * Tests for RelativeValidity instances
    , relativeValiditySpec
    , relativeValidityLaws
    , relativeValidityLawsForGens
    , relativeValidityImpliesValidA
    , relativeValidityImpliesValidB

    -- * Tests for GenRelativeValidity instances
    , genRelativeValiditySpec
    , genRelativeValidityValidGeneratesValid
    , genRelativeValidityInvalidGeneratesInvalid

    -- * Standard tests involving validity
    , producesValidsOnGen
    , producesValidsOnArbitrary
    , alwaysProducesValid
    , producesValidsOnValids
    , producesValidsOnGens2
    , alwaysProducesValid2
    , producesValidsOnValids2
    , producesValidsOnGens3
    , alwaysProducesValid3
    , producesValidsOnValids3

    -- * Standard tests involving functions that can fail

    , CanFail(..)
    , succeedsOnGen
    , succeedsOnValidInput
    , failsOnGen
    , failsOnInvalidInput
    , validIfSucceedsOnGen
    , validIfSucceedsOnValid
    , validIfSucceedsOnArbitrary
    , validIfSucceeds
    , succeedsOnGens2
    , succeedsOnValidInput2
    , failsOnGens2
    , failsOnInvalidInput2
    , validIfSucceedsOnGens2
    , validIfSucceeds2

    -- * Standard tests involving equivalence of functions

    , equivalentOnGen
    , equivalentOnValid
    , equivalent
    , equivalentOnGens2
    , equivalentOnValids2
    , equivalent2
    , equivalentWhenFirstSucceedsOnGen
    , equivalentWhenFirstSucceedsOnValid
    , equivalentWhenFirstSucceeds
    , equivalentWhenFirstSucceedsOnGens2
    , equivalentWhenFirstSucceedsOnValids2
    , equivalentWhenFirstSucceeds2
    , equivalentWhenSecondSucceedsOnGen
    , equivalentWhenSecondSucceedsOnValid
    , equivalentWhenSecondSucceeds
    , equivalentWhenSecondSucceedsOnGens2
    , equivalentWhenSecondSucceedsOnValids2
    , equivalentWhenSecondSucceeds2
    , equivalentWhenSucceedOnGen
    , equivalentWhenSucceedOnValid
    , equivalentWhenSucceed
    , equivalentWhenSucceedOnGens2
    , equivalentWhenSucceedOnValids2
    , equivalentWhenSucceed2

    -- * Standard tests involving inverse functions

    , inverseFunctionsOnGen
    , inverseFunctionsOnValid
    , inverseFunctions
    , inverseFunctionsOnArbitrary
    , inverseFunctionsIfFirstSucceedsOnGen
    , inverseFunctionsIfFirstSucceedsOnValid
    , inverseFunctionsIfFirstSucceeds
    , inverseFunctionsIfFirstSucceedsOnArbitrary
    , inverseFunctionsIfSecondSucceedsOnGen
    , inverseFunctionsIfSecondSucceedsOnValid
    , inverseFunctionsIfSecondSucceeds
    , inverseFunctionsIfSecondSucceedsOnArbitrary
    , inverseFunctionsIfSucceedOnGen
    , inverseFunctionsIfSucceedOnValid
    , inverseFunctionsIfSucceed
    , inverseFunctionsIfSucceedOnArbitrary

    -- * Properties of relations

    -- ** Reflexivity
    , reflexivityOnGen
    , reflexivityOnValid
    , reflexivity
    , reflexivityOnArbitrary

    -- ** Transitivity
    , transitivityOnGens
    , transitivityOnValid
    , transitivity
    , transitivityOnArbitrary

    -- ** Antisymmetry
    , antisymmetryOnGensWithEquality
    , antisymmetryOnGensEq
    , antisymmetryOnValid
    , antisymmetry
    , antisymmetryOnArbitrary

    -- ** Symmetry
    , symmetryOnGens
    , symmetryOnValid
    , symmetry
    , symmetryOnArbitrary

    -- * Properties of relations

    -- ** Identity element
    , leftIdentityOnGen
    , leftIdentityOnValid
    , leftIdentity
    , rightIdentityOnGen
    , rightIdentityOnValid
    , rightIdentity
    , identityOnGen
    , identityOnValid
    , identity

    -- ** Associativity
    , associativeOnGens
    , associativeOnValids
    , associative
    , associativeOnArbitrary

    -- ** Commutativity
    , commutativeOnGens
    , commutativeOnValids
    , commutative
    , commutativeOnArbitrary

    -- * Eq properties
    , eqSpec

    -- * Ord properties
    , ordSpec

    ) where

import           Data.Data
import           Data.Proxy

import           Data.GenRelativeValidity
import           Data.GenValidity

import           Test.Hspec
import           Test.QuickCheck

validitySpec
    :: (Typeable a, Show a, GenValidity a)
    => Proxy a
    -> Spec
validitySpec proxy = do
    let name = nameOf proxy
    describe ("Validity " ++ name) $ do
        it "is instantiated such that 'isValid' and `isInvalid` are complementary." $
            validityLaws proxy

validityLaws
    :: (Show a, GenValidity a)
    => Proxy a
    -> Property
validityLaws proxy = validityLawsForGen proxy genUnchecked

validityLawsForGen
    :: (Show a, Validity a)
    => Proxy a
    -> Gen a
    -> Property
validityLawsForGen proxy gen =
    forAll gen $ \a ->
        (a `asProxyTypeOf` proxy) `shouldSatisfy`
            (\v -> (isValid v || isInvalid v)
            && not (isValid v && isInvalid v))

-- | A @Spec@ that specifies that @arbitrary@ only generates data that
-- satisfy @isValid@ and that @shrink@ only produces data that satisfy
-- @isValid@.
--
-- Example usage:
--
-- > arbitrarySpec (Proxy :: Proxy MyData)
arbitrarySpec
    :: (Typeable a, Show a, Validity a, Arbitrary a)
    => Proxy a
    -> Spec
arbitrarySpec proxy = do
    let name = nameOf proxy
    describe ("Arbitrary " ++ name) $ do
        it ("is instantiated such that 'arbitrary' only generates valid \'"
            ++ name
            ++ "\'s.") $
            arbitraryGeneratesOnlyValid proxy

        it ("is instantiated such that 'shrink' only produces valid \'"
            ++ name
            ++ "\'s.") $ do
            forAll arbitrary $ \a ->
                shrink (a `asProxyTypeOf` proxy) `shouldSatisfy` all isValid

-- | @arbitrary@ only generates valid data
arbitraryGeneratesOnlyValid
    :: forall a. (Show a, Validity a, Arbitrary a)
    => Proxy a
    -> Property
arbitraryGeneratesOnlyValid _ =
    genGeneratesValid (arbitrary :: Gen a)

-- | @shrink@, applied to valid data only produces valid data
shrinkProducesOnlyValids
    :: forall a. (Show a, Validity a, Arbitrary a)
    => Proxy a
    -> Property
shrinkProducesOnlyValids _ =
    genGeneratesValid (shrink <$> arbitrary :: Gen [a])


-- | A @Spec@ that specifies that @genValid@ only generates valid data and that
-- @genInvalid@ only generates invalid data.
--
-- In general it is a good idea to add this spec to your test suite if you
-- write a custom implementation of @genValid@ or @genInvalid@.
--
-- Example usage:
--
-- > genValiditySpec (Proxy :: Proxy MyData)
genValiditySpec
    :: (Typeable a, Show a, GenValidity a)
    => Proxy a
    -> Spec
genValiditySpec proxy = do
    let name = nameOf proxy
    describe ("GenValidity " ++ name) $ do
        describe ("genValid   :: Gen " ++ name) $
            it ("only generates valid \'" ++ name ++ "\'s") $
                genValidityValidGeneratesValid proxy

        describe ("genInvalid :: Gen " ++ name) $
            it ("only generates invalid \'" ++ name ++ "\'s") $
                genValidityInvalidGeneratesInvalid proxy

-- | @genValid@ only generates valid data
genValidityValidGeneratesValid
    :: forall a. (Show a, GenValidity a)
    => Proxy a
    -> Property
genValidityValidGeneratesValid _ =
    genGeneratesValid (genValid :: Gen a)

-- | The given generator generates only valid data points
genGeneratesValid
    :: (Show a, Validity a)
    => Gen a
    -> Property
genGeneratesValid gen =
    forAll gen (`shouldSatisfy` isValid)


-- | @genValid@ only generates invalid data
genValidityInvalidGeneratesInvalid
    :: forall a. (Show a, GenValidity a)
    => Proxy a
    -> Property
genValidityInvalidGeneratesInvalid _ =
    genGeneratesInvalid (genInvalid :: Gen a)

-- | The given generator generates only invalid data points
genGeneratesInvalid
    :: (Show a, Validity a)
    => Gen a
    -> Property
genGeneratesInvalid gen =
    forAll gen (`shouldNotSatisfy` isValid)


-- | A @Spec@ that specifies that @isValidFor@ implies @isValid@
--
-- In general it is a good idea to add this spec to your test suite if
-- the @a@ and @b@ in @RelativeValidity a b@ also have a @Validity@ instance.
--
-- Example usage:
--
-- > relativeValiditySpec
-- >     (Proxy :: Proxy MyDataFor)
-- >     (Proxy :: Proxy MyOtherData)
relativeValiditySpec
    :: (Typeable a, Typeable b,
        Show a, Show b,
        GenValidity a, GenValidity b, GenRelativeValidity a b)
    => Proxy a
    -> Proxy b
    -> Spec
relativeValiditySpec one two = do
    let nameOne = nameOf one
        nameTwo = nameOf two
    describe ("RelativeValidity " ++ nameOne ++ " " ++ nameTwo) $ do
        it ("is instantiated such that 'isValidFor'"
            ++ "and 'isInvalidFor' are complementary") $
            relativeValidityLaws one two

        describe ("isValidFor :: "
                  ++ nameOne
                  ++ " -> "
                  ++ nameTwo
                  ++ " -> Bool") $ do
            it ("implies isValid " ++ nameOne ++ " for any " ++ nameTwo) $
                relativeValidityImpliesValidA one two
            it ("implies isValid " ++ nameTwo ++ " for any " ++ nameOne) $
                relativeValidityImpliesValidB one two

relativeValidityLaws
    :: (Show a, Show b, GenValidity a, GenValidity b, RelativeValidity a b)
    => Proxy a
    -> Proxy b
    -> Property
relativeValidityLaws proxya proxyb
    = relativeValidityLawsForGens proxya proxyb genUnchecked genUnchecked

relativeValidityLawsForGens
    :: (Show a, Show b, RelativeValidity a b)
    => Proxy a
    -> Proxy b
    -> Gen a
    -> Gen b
    -> Property
relativeValidityLawsForGens proxya proxyb gena genb =
    forAll gena $ \a ->
        forAll genb $ \b ->
            (a `asProxyTypeOf` proxya, b `asProxyTypeOf` proxyb)
                `shouldSatisfy`
                    (\(v, w) -> (isValidFor v w || isInvalidFor v w)
                    && not (isValidFor v w && isInvalidFor v w))

-- | @isValidFor a b@ implies @isValid a@ for all @b@
relativeValidityImpliesValidA
    :: (Show a, Show b,
        GenValidity a, GenValidity b, RelativeValidity a b)
    => Proxy a
    -> Proxy b
    -> Property
relativeValidityImpliesValidA one two =
    forAll genUnchecked $ \a ->
        forAll genUnchecked $ \b ->
            not ((a `asProxyTypeOf` one) `isValidFor` (b `asProxyTypeOf` two))
            || isValid a

-- | @isValidFor a b@ implies @isValid b@ for all @a@
relativeValidityImpliesValidB
    :: (Show a, Show b,
        GenValidity a, GenValidity b, RelativeValidity a b)
    => Proxy a
    -> Proxy b
    -> Property
relativeValidityImpliesValidB one two =
    forAll genUnchecked $ \a ->
        forAll genUnchecked $ \b ->
            not ((a `asProxyTypeOf` one) `isValidFor` (b `asProxyTypeOf` two))
              || isValid b


-- | A @Spec@ that specifies that @genValidFor@ and @genInvalidFor@ work as
-- intended.
--
-- In general it is a good idea to add this spec to your test suite if you
-- write a custom implementation of @genValidFor@ or @genInvalidFor@.
--
-- Example usage:
--
-- > relativeGenValiditySpec (proxy :: MyDataFor) (proxy :: MyOtherData)
genRelativeValiditySpec
    :: (Typeable a, Typeable b,
        Show a, Show b,
        GenValidity a, GenValidity b,
        RelativeValidity a b,
        GenRelativeValidity a b)
    => Proxy a
    -> Proxy b
    -> Spec
genRelativeValiditySpec one two = do
    let nameOne = nameOf one
    let nameTwo = nameOf two
    describe ("GenRelativeValidity " ++ nameOne ++ " " ++ nameTwo) $ do
        describe ("genValidFor   :: " ++ nameTwo ++ " -> Gen " ++ nameOne) $
            it ("only generates valid \'"
                ++ nameOne
                ++ "\'s for the "
                ++ nameTwo) $
                genRelativeValidityValidGeneratesValid one two

        describe ("genInvalidFor :: " ++ nameTwo ++ " -> Gen " ++ nameOne) $
            it ("only generates invalid \'"
                ++ nameOne
                ++ "\'s for the "
                ++ nameTwo) $
                genRelativeValidityInvalidGeneratesInvalid one two

-- | @genValidFor b@ only generates values that satisfy @isValidFor b@
genRelativeValidityValidGeneratesValid
    :: (Show a, Show b,
        GenValidity a, GenValidity b,
        RelativeValidity a b,
        GenRelativeValidity a b)
    => Proxy a
    -> Proxy b
    -> Property
genRelativeValidityValidGeneratesValid one two =
    forAll genValid $ \b ->
        forAll (genValidFor b) $ \a ->
            (a `asProxyTypeOf` one)
                `shouldSatisfy` (`isValidFor` (b `asProxyTypeOf` two))

-- | @genInvalidFor b@ only generates values that do not satisfy @isValidFor b@
genRelativeValidityInvalidGeneratesInvalid
    :: (Show a, Show b,
        GenValidity a, GenValidity b,
        RelativeValidity a b,
        GenRelativeValidity a b)
    => Proxy a
    -> Proxy b
    -> Property
genRelativeValidityInvalidGeneratesInvalid one two =
    forAll genUnchecked $ \b ->
        forAll (genInvalidFor b) $ \a ->
            (a `asProxyTypeOf` one)
                `shouldNotSatisfy` (`isValidFor` (b `asProxyTypeOf` two))

-- | A class of types that are the result of functions that can fail
--
-- You should not use this class yourself.
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

-- | The function produces valid output when the input is generated as
-- specified by the given generator.
producesValidsOnGen
    :: (Show a, Show b, Validity b)
    => (a -> b)
    -> Gen a
    -> Property
producesValidsOnGen func gen
    = forAll gen $ \a -> func a `shouldSatisfy` isValid

-- | The function produces valid output when the input is generated by
-- @arbitrary@
producesValidsOnArbitrary
    :: (Show a, Show b, Arbitrary a, Validity b)
    => (a -> b)
    -> Property
producesValidsOnArbitrary = (`producesValidsOnGen` arbitrary)

-- | The function produces valid output when the input is generated by
-- @genUnchecked@
alwaysProducesValid
    :: (Show a, Show b, GenValidity a, Validity b)
    => (a -> b)
    -> Property
alwaysProducesValid = (`producesValidsOnGen` genUnchecked)

-- | The function produces valid output when the input is generated by
-- @genValid@
producesValidsOnValids
    :: (Show a, Show b, GenValidity a, Validity b)
    => (a -> b)
    -> Property
producesValidsOnValids = (`producesValidsOnGen` genValid)

producesValidsOnGens2
    :: (Show a, Show b, Show c, Validity c)
    => (a -> b -> c)
    -> Gen (a, b)
    -> Property
producesValidsOnGens2 func gen
    = forAll gen $ \(a, b) ->
        func a b `shouldSatisfy` isValid

alwaysProducesValid2
    :: (Show a, Show b, Show c, GenValidity a, GenValidity b, Validity c)
    => (a -> b -> c)
    -> Property
alwaysProducesValid2 func
    = producesValidsOnGens2 func genUnchecked

producesValidsOnValids2
    :: (Show a, Show b, Show c, GenValidity a, GenValidity b, Validity c)
    => (a -> b -> c)
    -> Property
producesValidsOnValids2 func
    = producesValidsOnGens2 func genValid

producesValidsOnGens3
    :: (Show a, Show b, Show c, Show d, Validity d)
    => (a -> b -> c -> d)
    -> Gen (a, b, c)
    -> Property
producesValidsOnGens3 func gen
    = forAll gen $ \(a, b, c) ->
        func a b c `shouldSatisfy` isValid

alwaysProducesValid3
    :: (Show a, Show b, Show c, Show d,
        GenValidity a, GenValidity b, GenValidity c,
        Validity d)
    => (a -> b -> c -> d)
    -> Property
alwaysProducesValid3 func
    = producesValidsOnGens3 func genUnchecked

producesValidsOnValids3
    :: (Show a, Show b, Show c, Show d,
        GenValidity a, GenValidity b, GenValidity c,
        Validity d)
    => (a -> b -> c -> d)
    -> Property
producesValidsOnValids3 func
    = producesValidsOnGens3 func genValid

-- | The function succeeds if the input is generated by the given generator
succeedsOnGen
    :: (Show a, Show b, Show (f b), CanFail f)
    => (a -> f b)
    -> Gen a
    -> Property
succeedsOnGen func gen
    = forAll gen $ \a -> func a `shouldNotSatisfy` hasFailed

-- | The function succeeds if the input is generated by @genValid@
succeedsOnValidInput
    :: (Show a, Show b, Show (f b), GenValidity a, CanFail f)
    => (a -> f b)
    -> Property
succeedsOnValidInput = (`succeedsOnGen` genValid)

-- | The function fails if the input is generated by the given generator
failsOnGen
    :: (Show a, Show b, Show (f b), CanFail f)
    => (a -> f b)
    -> Gen a
    -> Property
failsOnGen func gen
    = forAll gen $ \a -> func a `shouldSatisfy` hasFailed

-- | The function fails if the input is generated by @genInvalid@
failsOnInvalidInput
    :: (Show a, Show b, Show (f b), GenValidity a, CanFail f)
    => (a -> f b)
    -> Property
failsOnInvalidInput = (`failsOnGen` genInvalid)

-- | The function produces output that satisfies @isValid@ if it is given input
-- that is generated by the given generator.
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

-- | The function produces output that satisfies @isValid@ if it is given input
-- that is generated by @arbitrary@.
validIfSucceedsOnValid
    :: (Show a, Show b, Show (f b), GenValidity a, Validity b, CanFail f)
    => (a -> f b)
    -> Property
validIfSucceedsOnValid = (`validIfSucceedsOnGen` genValid)


-- | The function produces output that satisfies @isValid@ if it is given input
-- that is generated by @arbitrary@.
validIfSucceedsOnArbitrary
    :: (Show a, Show b, Show (f b), Arbitrary a, Validity b, CanFail f)
    => (a -> f b)
    -> Property
validIfSucceedsOnArbitrary = (`validIfSucceedsOnGen` arbitrary)

-- | The function produces output that satisfies @isValid@ if it is given input
-- that is generated by @genUnchecked@.
validIfSucceeds
    :: (Show a, Show b, Show (f b), GenValidity a, Validity b, CanFail f)
    => (a -> f b)
    -> Property
validIfSucceeds = (`validIfSucceedsOnGen` genUnchecked)


succeedsOnGens2
    :: (Show a, Show b, Show c, Show (f c),
        CanFail f)
    => (a -> b -> f c)
    -> Gen a -> Gen b
    -> Property
succeedsOnGens2 func genA genB =
    forAll genA $ \a ->
        forAll genB $ \b ->
            func a b `shouldNotSatisfy` hasFailed


succeedsOnValidInput2
    :: (Show a, Show b, Show c, Show (f c),
        GenValidity a, GenValidity b,
        CanFail f)
    => (a -> b -> f c)
    -> Property
succeedsOnValidInput2 func
    = succeedsOnGens2 func genValid genValid


failsOnGens2
    :: (Show a, Show b, Show c, Show (f c),
        CanFail f)
    => (a -> b -> f c)
    -> Gen a -> Gen b
    -> Property
failsOnGens2 func genA genB =
    forAll genA $ \a ->
        forAll genB $ \b ->
            func a b `shouldSatisfy` hasFailed


failsOnInvalidInput2
    :: (Show a, Show b, Show c, Show (f c),
        GenValidity a, GenValidity b,
        CanFail f)
    => (a -> b -> f c)
    -> Property
failsOnInvalidInput2 func
    =    failsOnGens2 func genInvalid genUnchecked
    .&&. failsOnGens2 func genUnchecked genInvalid


validIfSucceedsOnGens2
    :: (Show a, Show b, Show c, Show (f c),
        Validity c, CanFail f)
    => (a -> b -> f c)
    -> Gen a -> Gen b
    -> Property
validIfSucceedsOnGens2 func genA genB =
    forAll genA $ \a ->
        forAll genB $ \b ->
            case resultIfSucceeded (func a b) of
                Nothing  -> return () -- Can happen
                Just res -> res `shouldSatisfy` isValid


validIfSucceeds2
  :: (Show a, Show b, Show c, Show (f c),
      GenValidity a, GenValidity b,
      Validity c, CanFail f)
  => (a -> b -> f c)
  -> Property
validIfSucceeds2 func
    = validIfSucceedsOnGens2 func genUnchecked genUnchecked

equivalentOnGen
  :: (Show a, Eq a, Show b, Eq b)
  => (a -> b)
  -> (a -> b)
  -> Gen a
  -> Property
equivalentOnGen f g gen =
    forAll gen $ \a ->
        f a `shouldBe` g a

equivalentOnValid
  :: (Show a, Eq a, GenValidity a, Show b, Eq b)
  => (a -> b)
  -> (a -> b)
  -> Property
equivalentOnValid f g
    = equivalentOnGen f g genValid

equivalent
  :: (Show a, Eq a, GenValidity a, Show b, Eq b)
  => (a -> b)
  -> (a -> b)
  -> Property
equivalent f g
    = equivalentOnGen f g genUnchecked

equivalentOnGens2
  :: (Show a, Eq a,
      Show b, Eq b,
      Show c, Eq c)
  => (a -> b -> c)
  -> (a -> b -> c)
  -> Gen (a, b)
  -> Property
equivalentOnGens2 f g gen =
    forAll gen $ \(a, b) ->
        f a b `shouldBe` g a b

equivalentOnValids2
  :: (Show a, Eq a, GenValidity a,
      Show b, Eq b, GenValidity b,
      Show c, Eq c)
  => (a -> b -> c)
  -> (a -> b -> c)
  -> Property
equivalentOnValids2 f g
    = equivalentOnGens2 f g genValid

equivalent2
  :: (Show a, Eq a, GenValidity a,
      Show b, Eq b, GenValidity b,
      Show c, Eq c)
  => (a -> b -> c)
  -> (a -> b -> c)
  -> Property
equivalent2 f g
    = equivalentOnGens2 f g genUnchecked

equivalentWhenFirstSucceedsOnGen
  :: (Show a, Eq a, Show b, Eq b, CanFail f)
  => (a -> f b)
  -> (a -> b)
  -> Gen a
  -> Property
equivalentWhenFirstSucceedsOnGen f g gen =
    forAll gen $ \a ->
        case resultIfSucceeded (f a) of
            Nothing -> return () -- fine
            Just r  -> r `shouldBe` g a

equivalentWhenFirstSucceedsOnValid
  :: (Show a, Eq a, GenValidity a, Show b, Eq b, CanFail f)
  => (a -> f b)
  -> (a -> b)
  -> Property
equivalentWhenFirstSucceedsOnValid f g
    = equivalentWhenFirstSucceedsOnGen f g genValid

equivalentWhenFirstSucceeds
  :: (Show a, Eq a, GenValidity a, Show b, Eq b, CanFail f)
  => (a -> f b)
  -> (a -> b)
  -> Property
equivalentWhenFirstSucceeds f g
    = equivalentWhenFirstSucceedsOnGen f g genUnchecked

equivalentWhenFirstSucceedsOnGens2
  :: (Show a, Eq a,
      Show b, Eq b,
      Show c, Eq c,
      CanFail f)
  => (a -> b -> f c)
  -> (a -> b -> c)
  -> Gen (a, b)
  -> Property
equivalentWhenFirstSucceedsOnGens2 f g gen =
    forAll gen $ \(a, b) ->
        case resultIfSucceeded (f a b) of
            Nothing -> return () -- fine
            Just rs -> rs `shouldBe` g a b

equivalentWhenFirstSucceedsOnValids2
  :: (Show a, Eq a, GenValidity a,
      Show b, Eq b, GenValidity b,
      Show c, Eq c,
      CanFail f)
  => (a -> b -> f c)
  -> (a -> b -> c)
  -> Property
equivalentWhenFirstSucceedsOnValids2 f g
    = equivalentWhenFirstSucceedsOnGens2 f g genValid

equivalentWhenFirstSucceeds2
  :: (Show a, Eq a, GenValidity a,
      Show b, Eq b, GenValidity b,
      Show c, Eq c,
      CanFail f)
  => (a -> b -> f c)
  -> (a -> b -> c)
  -> Property
equivalentWhenFirstSucceeds2 f g
    = equivalentWhenFirstSucceedsOnGens2 f g genUnchecked

equivalentWhenSecondSucceedsOnGen
  :: (Show a, Eq a, Show b, Eq b, CanFail f)
  => (a -> b)
  -> (a -> f b)
  -> Gen a
  -> Property
equivalentWhenSecondSucceedsOnGen f g gen =
    forAll gen $ \a ->
        case resultIfSucceeded (g a) of
            Nothing -> return () -- fine
            Just r  -> r `shouldBe` f a

equivalentWhenSecondSucceedsOnValid
  :: (Show a, Eq a, GenValidity a, Show b, Eq b, CanFail f)
  => (a -> b)
  -> (a -> f b)
  -> Property
equivalentWhenSecondSucceedsOnValid f g
    = equivalentWhenSecondSucceedsOnGen f g genValid

equivalentWhenSecondSucceeds
  :: (Show a, Eq a, GenValidity a, Show b, Eq b, CanFail f)
  => (a -> b)
  -> (a -> f b)
  -> Property
equivalentWhenSecondSucceeds f g
    = equivalentWhenSecondSucceedsOnGen f g genUnchecked

equivalentWhenSecondSucceedsOnGens2
  :: (Show a, Eq a,
      Show b, Eq b,
      Show c, Eq c,
      CanFail f)
  => (a -> b -> c)
  -> (a -> b -> f c)
  -> Gen (a, b)
  -> Property
equivalentWhenSecondSucceedsOnGens2 f g gen =
    forAll gen $ \(a, b) ->
        case resultIfSucceeded (g a b) of
            Nothing -> return () -- fine
            Just rs -> rs `shouldBe` f a b

equivalentWhenSecondSucceedsOnValids2
  :: (Show a, Eq a, GenValidity a,
      Show b, Eq b, GenValidity b,
      Show c, Eq c,
      CanFail f)
  => (a -> b -> c)
  -> (a -> b -> f c)
  -> Property
equivalentWhenSecondSucceedsOnValids2 f g
    = equivalentWhenSecondSucceedsOnGens2 f g genValid

equivalentWhenSecondSucceeds2
  :: (Show a, Eq a, GenValidity a,
      Show b, Eq b, GenValidity b,
      Show c, Eq c,
      CanFail f)
  => (a -> b -> c)
  -> (a -> b -> f c)
  -> Property
equivalentWhenSecondSucceeds2 f g
    = equivalentWhenSecondSucceedsOnGens2 f g genUnchecked

equivalentWhenSucceedOnGen
  :: (Show a, Eq a, Show b, Eq b, CanFail f)
  => (a -> f b)
  -> (a -> f b)
  -> Gen a
  -> Property
equivalentWhenSucceedOnGen f g gen =
    forAll gen $ \a ->
        case do fa <- resultIfSucceeded $ f a
                ga <- resultIfSucceeded $ g a
                return $ (fa, ga)
            of
            Nothing -> return () -- fine
            Just (fa, ga)  -> fa `shouldBe` ga

equivalentWhenSucceedOnValid
  :: (Show a, Eq a, GenValidity a, Show b, Eq b, CanFail f)
  => (a -> f b)
  -> (a -> f b)
  -> Property
equivalentWhenSucceedOnValid f g
    = equivalentWhenSucceedOnGen f g genValid

equivalentWhenSucceed
  :: (Show a, Eq a, GenValidity a, Show b, Eq b, CanFail f)
  => (a -> f b)
  -> (a -> f b)
  -> Property
equivalentWhenSucceed f g
    = equivalentWhenSucceedOnGen f g genUnchecked

equivalentWhenSucceedOnGens2
  :: (Show a, Eq a,
      Show b, Eq b,
      Show c, Eq c,
      CanFail f)
  => (a -> b -> f c)
  -> (a -> b -> f c)
  -> Gen (a, b)
  -> Property
equivalentWhenSucceedOnGens2 f g gen =
    forAll gen $ \(a, b) ->
        case do fab <- resultIfSucceeded $ f a b
                gab <- resultIfSucceeded $ g a b
                return (fab, gab)
                of
            Nothing -> return () -- fine
            Just (fab, gab) -> fab `shouldBe` gab

equivalentWhenSucceedOnValids2
  :: (Show a, Eq a, GenValidity a,
      Show b, Eq b, GenValidity b,
      Show c, Eq c,
      CanFail f)
  => (a -> b -> f c)
  -> (a -> b -> f c)
  -> Property
equivalentWhenSucceedOnValids2 f g
    = equivalentWhenSucceedOnGens2 f g genValid

equivalentWhenSucceed2
  :: (Show a, Eq a, GenValidity a,
      Show b, Eq b, GenValidity b,
      Show c, Eq c,
      CanFail f)
  => (a -> b -> f c)
  -> (a -> b -> f c)
  -> Property
equivalentWhenSucceed2 f g
    = equivalentWhenSucceedOnGens2 f g genUnchecked

inverseFunctionsOnGen
  :: (Show a, Eq a)
  => (a -> b)
  -> (b -> a)
  -> Gen a
  -> Property
inverseFunctionsOnGen f g gen =
    forAll gen $ \a ->
      g (f a) `shouldBe` a


inverseFunctionsOnValid
  :: (Show a, Eq a, GenValidity a)
  => (a -> b)
  -> (b -> a)
  -> Property
inverseFunctionsOnValid f g
    = inverseFunctionsOnGen f g genValid


inverseFunctions
  :: (Show a, Eq a, GenValidity a)
  => (a -> b)
  -> (b -> a)
  -> Property
inverseFunctions f g
    = inverseFunctionsOnGen f g genUnchecked

inverseFunctionsOnArbitrary
  :: (Show a, Eq a, Arbitrary a)
  => (a -> b)
  -> (b -> a)
  -> Property
inverseFunctionsOnArbitrary f g
    = inverseFunctionsOnGen f g arbitrary


inverseFunctionsIfFirstSucceedsOnGen
  :: (Show a, Eq a, CanFail f)
  => (a -> f b)
  -> (b -> a)
  -> Gen a
  -> Property
inverseFunctionsIfFirstSucceedsOnGen f g gen =
    forAll gen $ \a ->
      case resultIfSucceeded (f a) of
          Nothing -> return () -- fine
          Just b  -> g b `shouldBe` a


inverseFunctionsIfFirstSucceedsOnValid
  :: (Show a, Eq a, GenValidity a, CanFail f)
  => (a -> f b)
  -> (b -> a)
  -> Property
inverseFunctionsIfFirstSucceedsOnValid f g
    = inverseFunctionsIfFirstSucceedsOnGen f g genValid


inverseFunctionsIfFirstSucceeds
  :: (Show a, Eq a, GenValidity a, CanFail f)
  => (a -> f b)
  -> (b -> a)
  -> Property
inverseFunctionsIfFirstSucceeds f g
    = inverseFunctionsIfFirstSucceedsOnGen f g genUnchecked

inverseFunctionsIfFirstSucceedsOnArbitrary
  :: (Show a, Eq a, Arbitrary a, CanFail f)
  => (a -> f b)
  -> (b -> a)
  -> Property
inverseFunctionsIfFirstSucceedsOnArbitrary f g
    = inverseFunctionsIfFirstSucceedsOnGen f g arbitrary


inverseFunctionsIfSecondSucceedsOnGen
  :: (Show a, Eq a, CanFail f)
  => (a -> b)
  -> (b -> f a)
  -> Gen a
  -> Property
inverseFunctionsIfSecondSucceedsOnGen f g gen =
    forAll gen $ \a ->
      case resultIfSucceeded (g (f a)) of
          Nothing -> return () -- fine
          Just r  -> r `shouldBe` a


inverseFunctionsIfSecondSucceedsOnValid
  :: (Show a, Eq a, GenValidity a, CanFail f)
  => (a -> b)
  -> (b -> f a)
  -> Property
inverseFunctionsIfSecondSucceedsOnValid f g
    = inverseFunctionsIfSecondSucceedsOnGen f g genValid


inverseFunctionsIfSecondSucceeds
  :: (Show a, Eq a, GenValidity a, CanFail f)
  => (a -> b)
  -> (b -> f a)
  -> Property
inverseFunctionsIfSecondSucceeds f g
    = inverseFunctionsIfSecondSucceedsOnGen f g genUnchecked

inverseFunctionsIfSecondSucceedsOnArbitrary
  :: (Show a, Eq a, Arbitrary a, CanFail f)
  => (a -> b)
  -> (b -> f a)
  -> Property
inverseFunctionsIfSecondSucceedsOnArbitrary f g
    = inverseFunctionsIfSecondSucceedsOnGen f g arbitrary


inverseFunctionsIfSucceedOnGen
  :: (Show a, Eq a, CanFail f, CanFail g)
  => (a -> f b)
  -> (b -> g a)
  -> Gen a
  -> Property
inverseFunctionsIfSucceedOnGen f g gen =
    forAll gen $ \a ->
      case do fa <- resultIfSucceeded $ f a
              resultIfSucceeded $ g fa
          of
          Nothing -> return () -- fine
          Just r  -> r `shouldBe` a


inverseFunctionsIfSucceedOnValid
  :: (Show a, Eq a, GenValidity a, CanFail f, CanFail g)
  => (a -> f b)
  -> (b -> g a)
  -> Property
inverseFunctionsIfSucceedOnValid f g
    = inverseFunctionsIfSucceedOnGen f g genValid

inverseFunctionsIfSucceed
  :: (Show a, Eq a, GenValidity a, CanFail f, CanFail g)
  => (a -> f b)
  -> (b -> g a)
  -> Property
inverseFunctionsIfSucceed f g
    = inverseFunctionsIfSucceedOnGen f g genUnchecked

inverseFunctionsIfSucceedOnArbitrary
  :: (Show a, Eq a, Arbitrary a, CanFail f, CanFail g)
  => (a -> f b)
  -> (b -> g a)
  -> Property
inverseFunctionsIfSucceedOnArbitrary f g
    = inverseFunctionsIfSucceedOnGen f g arbitrary

reflexivityOnGen
  :: Show a
  => (a -> a -> Bool)
  -> Gen a
  -> Property
reflexivityOnGen func gen =
    forAll gen $ \a ->
        func a a

reflexivityOnValid
  :: (Show a, GenValidity a)
  => (a -> a -> Bool)
  -> Property
reflexivityOnValid func
    = reflexivityOnGen func genValid

reflexivity
  :: (Show a, GenValidity a)
  => (a -> a -> Bool)
  -> Property
reflexivity func
    = reflexivityOnGen func genUnchecked

reflexivityOnArbitrary
  :: (Show a, Arbitrary a)
  => (a -> a -> Bool)
  -> Property
reflexivityOnArbitrary func
    = reflexivityOnGen func arbitrary


transitivityOnGens
  :: Show a
  => (a -> a -> Bool)
  -> Gen (a, a, a)
  -> Property
transitivityOnGens func gen =
    forAll gen $ \(a, b, c) ->
        (func a b && func b c)
        ===> (func a c)

transitivityOnValid
  :: (Show a, GenValidity a)
  => (a -> a -> Bool)
  -> Property
transitivityOnValid func
    = transitivityOnGens func genValid

transitivity
  :: (Show a, GenValidity a)
  => (a -> a -> Bool)
  -> Property
transitivity func
    = transitivityOnGens func genUnchecked

transitivityOnArbitrary
  :: (Show a, Arbitrary a)
  => (a -> a -> Bool)
  -> Property
transitivityOnArbitrary func
    = transitivityOnGens func arbitrary

antisymmetryOnGensWithEquality
  :: Show a
  => (a -> a -> Bool)
  -> Gen (a, a)
  -> (a -> a -> Bool)
  -> Property
antisymmetryOnGensWithEquality func gen eq =
    forAll gen $ \(a, b) ->
        (func a b && func b a)
        ===> (a `eq` b)

antisymmetryOnGensEq
  :: (Show a, Eq a)
  => (a -> a -> Bool)
  -> Gen (a, a)
  -> Property
antisymmetryOnGensEq func gen
    = antisymmetryOnGensWithEquality func gen (==)

antisymmetryOnValid
  :: (Show a, Eq a, GenValidity a)
  => (a -> a -> Bool)
  -> Property
antisymmetryOnValid func =
    antisymmetryOnGensEq func genValid

antisymmetry
  :: (Show a, Eq a, GenValidity a)
  => (a -> a -> Bool)
  -> Property
antisymmetry func =
    antisymmetryOnGensEq func genUnchecked

antisymmetryOnArbitrary
  :: (Show a, Eq a, Arbitrary a)
  => (a -> a -> Bool)
  -> Property
antisymmetryOnArbitrary func =
    antisymmetryOnGensEq func arbitrary

symmetryOnGens
  :: Show a
  => (a -> a -> Bool)
  -> Gen (a, a)
  -> Property
symmetryOnGens func gen =
    forAll gen $ \(a, b) ->
        (func a b <==> func b a)

symmetryOnValid
  :: (Show a, GenValidity a)
  => (a -> a -> Bool)
  -> Property
symmetryOnValid func =
    symmetryOnGens func genValid

symmetry
  :: (Show a, GenValidity a)
  => (a -> a -> Bool)
  -> Property
symmetry func =
    symmetryOnGens func genUnchecked

symmetryOnArbitrary
  :: (Show a, Arbitrary a)
  => (a -> a -> Bool)
  -> Property
symmetryOnArbitrary func =
    symmetryOnGens func arbitrary


leftIdentityOnGen
  :: (Show a, Eq a)
  => (b -> a -> a)
  -> b
  -> Gen a
  -> Property
leftIdentityOnGen op b gen =
    forAll gen $ \a ->
        b `op` a `shouldBe` a

leftIdentityOnValid
  :: (Show a, Eq a, GenValidity a)
  => (b -> a -> a)
  -> b
  -> Property
leftIdentityOnValid op b
    = leftIdentityOnGen op b genValid

leftIdentity
  :: (Show a, Eq a, GenValidity a)
  => (b -> a -> a)
  -> b
  -> Property
leftIdentity op b
    = leftIdentityOnGen op b genUnchecked

rightIdentityOnGen
  :: (Show a, Eq a)
  => (a -> b -> a)
  -> b
  -> Gen a
  -> Property
rightIdentityOnGen op b gen =
    forAll gen $ \a ->
        a `op` b `shouldBe` a

rightIdentityOnValid
  :: (Show a, Eq a, GenValidity a)
  => (a -> b -> a)
  -> b
  -> Property
rightIdentityOnValid op b
    = rightIdentityOnGen op b genValid

rightIdentity
  :: (Show a, Eq a, GenValidity a)
  => (a -> b -> a)
  -> b
  -> Property
rightIdentity op b
    = rightIdentityOnGen op b genUnchecked

identityOnGen
  :: (Show a, Eq a)
  => (a -> a -> a)
  -> a
  -> Gen a
  -> Property
identityOnGen op e gen =
    leftIdentityOnGen op e gen .&&. rightIdentityOnGen op e gen

identityOnValid
  :: (Show a, Eq a, GenValidity a)
  => (a -> a -> a)
  -> a
  -> Property
identityOnValid op a
    = identityOnGen op a genValid

identity
  :: (Show a, Eq a, GenValidity a)
  => (a -> a -> a)
  -> a
  -> Property
identity op e
    = identityOnGen op e genUnchecked

associativeOnGens
  :: (Show a, Eq a)
  => (a -> a -> a)
  -> Gen (a, a, a)
  -> Property
associativeOnGens op gen =
    forAll gen $ \(a, b, c) ->
        ((a `op` b) `op` c) `shouldBe` (a `op` (b `op` c))

associativeOnValids
  :: (Show a, Eq a, GenValidity a)
  => (a -> a -> a)
  -> Property
associativeOnValids op
    = associativeOnGens op genValid

associativeOnArbitrary
  :: (Show a, Eq a, Arbitrary a)
  => (a -> a -> a)
  -> Property
associativeOnArbitrary op
    = associativeOnGens op arbitrary

associative
  :: (Show a, Eq a, GenValidity a)
  => (a -> a -> a)
  -> Property
associative op
    = associativeOnGens op genUnchecked

commutativeOnGens
  :: (Show a, Eq a)
  => (a -> a -> a)
  -> Gen (a, a)
  -> Property
commutativeOnGens op gen =
    forAll gen $ \(a, b) ->
        (a `op` b) `shouldBe` (b `op` a)

commutativeOnValids
  :: (Show a, Eq a, GenValidity a)
  => (a -> a -> a)
  -> Property
commutativeOnValids op
    = commutativeOnGens op genValid

commutative
  :: (Show a, Eq a, GenValidity a)
  => (a -> a -> a)
  -> Property
commutative op
    = commutativeOnGens op genUnchecked

commutativeOnArbitrary
  :: (Show a, Eq a, Arbitrary a)
  => (a -> a -> a)
  -> Property
commutativeOnArbitrary op
    = commutativeOnGens op arbitrary

eqSpec
  :: (Show a, Eq a, Typeable a, GenValidity a)
  => Proxy a
  -> Spec
eqSpec proxy = do
    let name = nameOf proxy
        funeqstr = unwords
          [ "(==) ::"
          , name
          , "->"
          , name
          , "-> Bool"
          ]
        eq = \a b -> a `asProxyTypeOf` proxy == b
    describe ("Eq " ++ name) $ do
        it ("is instantated such that "
            ++ funeqstr
            ++ " is reflexive") $
            reflexivity eq

        it ("is instantated such that "
            ++ funeqstr
            ++ " is symmetric") $
            symmetry eq

        it ("is instantated such that "
            ++ funeqstr
            ++ " is transitive") $
            transitivity eq

ordSpec
  :: (Show a, Eq a, Ord a, Typeable a, GenValidity a)
  => Proxy a
  -> Spec
ordSpec proxy = do
    let name = nameOf proxy
        funlestr = unwords
          [ "(<=) ::"
          , name
          , "->"
          , name
          , "-> Ordering"
          ]
        cmp = \a b -> a `asProxyTypeOf` proxy <= b
    describe ("Ord " ++ name) $ do
        it ("is instantated such that "
            ++ funlestr
            ++ " is reflexive") $
            reflexivity cmp

        it ("is instantated such that "
            ++ funlestr
            ++ " is antisymmetric") $
            antisymmetry cmp

        it ("is instantated such that "
            ++ funlestr
            ++ " is transitive") $
            transitivity cmp


(===>) :: Bool -> Bool -> Bool
(===>) a b = not a || b

(<==>) :: Bool -> Bool -> Bool
(<==>) a b = a ===> b && b ===> a

nameOf :: Typeable a => Proxy a -> String
nameOf proxy =
    let (_, [ty]) = splitTyConApp $ typeOf proxy
    in show ty
