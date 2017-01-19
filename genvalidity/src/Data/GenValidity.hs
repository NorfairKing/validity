{-|

    @GenValidity@ exists to make tests involving @Validity@ types easier and speed
    up the generation of data for them.

    Let's use the example from @Data.Validity@ again: A datatype that represents
    primes.
    To implement tests for this datatype, we would have to be able to generate
    both primes and non-primes. We could do this with
    @(Prime <$> arbitrary) `suchThat` isValid@
    but this is tedious and inefficient.

    The @GenValid@ type class allows you to specify how to (efficiently)
    generate valid data of the given type to allow for easier and quicker testing.
    Just instantiating @GenUnchecked@ already gives you access to a default instance
    of @GenValid@ and @GenInvalid@ but writing custom implementations of these functions
    may speed up the generation of data.

    For example, to generate primes, we don't have to consider even numbers other
    than 2. A more efficient implementation could then look as follows:

    > instance GenUnchecked Prime where
    >     genUnchecked = Prime <$> arbitrary

    > instance GenValid Prime where
    >     genValid = Prime <$>
    >        (oneof
    >          [ pure 2
    >          , ((\y -> 2 * abs y + 1) <$> arbitrary) `suchThat` isPrime)
    >          ])


    Typical examples of tests involving validity could look as follows:

    > it "succeeds when given valid input" $ do
    >     forAll genValid $ \input ->
    >         myFunction input `shouldSatisfy` isRight

    > it "produces valid output when it succeeds" $ do
    >     forAll genUnchecked $ \input ->
    >         case myFunction input of
    >             Nothing -> return () -- Can happen
    >             Just output -> output `shouldSatisfy` isValid
    -}
module Data.GenValidity
    ( module Data.Validity
    , module Data.GenValidity
    ) where

import Data.Validity

import Test.QuickCheck

import Control.Monad (forM)

-- | A class of types for which truly arbitrary values can be generated.
class GenUnchecked a where
    genUnchecked :: Gen a

-- | A class of types for which valid values can be generated.
--
-- If you also write @Arbitrary@ instances for @GenValid@ types, it may be
-- best to simply write @arbitrary = genValid@.
class (Validity a, GenUnchecked a) =>
      GenValid a where
    genValid :: Gen a
    -- | Generate a valid datum, this should cover all possible valid values in
    -- the type
    --
    -- The default implementation is as follows:
    --
    -- >  genValid = genUnchecked `suchThat` isValid
    --
    -- To speed up testing, it may be a good idea to implement this yourself.
    -- If you do, make sure that it is possible to generate all possible valid
    -- data, otherwise your testing may not cover all cases.
    genValid = genUnchecked `suchThat` isValid

-- | A class of types for which invalid values can be generated.
class (Validity a, GenUnchecked a) =>
      GenInvalid a where
    genInvalid :: Gen a
    -- | Generate an invalid datum, this should cover all possible invalid
    -- values
    --
    -- > genInvalid = genUnchecked `suchThat` (not . isValid)
    --
    -- To speed up testing, it may be a good idea to implement this yourself.
    -- If you do, make sure that it is possible to generate all possible
    -- invalid data, otherwise your testing may not cover all cases.
    genInvalid = genUnchecked `suchThat` (not . isValid)

instance (GenUnchecked a, GenUnchecked b) =>
         GenUnchecked (a, b) where
    genUnchecked =
        sized $ \n -> do
            (r, s) <- genSplit n
            a <- resize r genUnchecked
            b <- resize s genUnchecked
            return (a, b)

instance (GenValid a, GenValid b) =>
         GenValid (a, b) where
    genValid =
        sized $ \n -> do
            (r, s) <- genSplit n
            a <- resize r genValid
            b <- resize s genValid
            return (a, b)

instance (GenInvalid a, GenInvalid b) =>
         GenInvalid (a, b) where
    genInvalid =
        sized $ \n -> do
            (r, s) <- genSplit n
            oneof
                [ do a <- resize r genUnchecked
                     b <- resize s genInvalid
                     return (a, b)
                , do a <- resize r genInvalid
                     b <- resize s genUnchecked
                     return (a, b)
                ]

instance (GenUnchecked a, GenUnchecked b) =>
         GenUnchecked (Either a b) where
    genUnchecked = oneof [Left <$> genUnchecked, Right <$> genUnchecked]

instance (GenValid a, GenValid b) =>
         GenValid (Either a b) where
    genValid = oneof [Left <$> genValid, Right <$> genValid]

-- | This instance ensures that the generated tupse contains at least one invalid element. The other element is unchecked.
instance (GenInvalid a, GenInvalid b) =>
         GenInvalid (Either a b) where
    genInvalid = oneof [Left <$> genInvalid, Right <$> genInvalid]

instance (GenUnchecked a, GenUnchecked b, GenUnchecked c) =>
         GenUnchecked (a, b, c) where
    genUnchecked =
        sized $ \n -> do
            (r, s, t) <- genSplit3 n
            a <- resize r genUnchecked
            b <- resize s genUnchecked
            c <- resize t genUnchecked
            return (a, b, c)

instance (GenValid a, GenValid b, GenValid c) =>
         GenValid (a, b, c) where
    genValid =
        sized $ \n -> do
            (r, s, t) <- genSplit3 n
            a <- resize r genValid
            b <- resize s genValid
            c <- resize t genValid
            return (a, b, c)

-- | This instance ensures that the generated triple contains at least one invalid element. The other two are unchecked.
instance (GenInvalid a, GenInvalid b, GenInvalid c) =>
         GenInvalid (a, b, c) where
    genInvalid =
        sized $ \n -> do
            (r, s, t) <- genSplit3 n
            oneof
                [ do a <- resize r genInvalid
                     b <- resize s genUnchecked
                     c <- resize t genUnchecked
                     return (a, b, c)
                , do a <- resize r genUnchecked
                     b <- resize s genInvalid
                     c <- resize t genUnchecked
                     return (a, b, c)
                , do a <- resize r genUnchecked
                     b <- resize s genUnchecked
                     c <- resize t genInvalid
                     return (a, b, c)
                ]

instance GenUnchecked a =>
         GenUnchecked (Maybe a) where
    genUnchecked = oneof [pure Nothing, Just <$> genUnchecked]

instance GenValid a =>
         GenValid (Maybe a) where
    genValid = oneof [pure Nothing, Just <$> genValid]

instance GenInvalid a =>
         GenInvalid (Maybe a) where
    genInvalid = Just <$> genInvalid

instance GenUnchecked a =>
         GenUnchecked [a] where
    genUnchecked = genListOf genUnchecked

-- | If we can generate values of a certain type, we can also generate lists of
-- them.
instance GenValid a =>
         GenValid [a] where
    genValid = genListOf genValid

-- | This instance ensures that the generated list contains at least one element
-- that satisfies 'isInvalid'. The rest is unchecked.
instance GenInvalid a =>
         GenInvalid [a] where
    genInvalid =
        sized $ \n -> do
            (x, y, z) <- genSplit3 n
            before <- resize x $ genListOf genUnchecked
            middle <- resize y genInvalid
            after <- resize z $ genListOf genUnchecked
            return $ before ++ [middle] ++ after

instance GenUnchecked () where
    genUnchecked = arbitrary

instance GenValid ()

instance GenUnchecked Bool where
    genUnchecked = arbitrary

instance GenValid Bool

instance GenUnchecked Ordering where
    genUnchecked = arbitrary

instance GenValid Ordering

instance GenUnchecked Char where
    genUnchecked = arbitrary

instance GenValid Char

instance GenUnchecked Int where
    genUnchecked = arbitrary

instance GenValid Int

instance GenUnchecked Word where
    genUnchecked = arbitrary

instance GenValid Word

instance GenUnchecked Float where
    genUnchecked = arbitrary

instance GenValid Float where
    genValid = arbitrary

-- | Either 'NaN' or 'Infinity'.
instance GenInvalid Float where
    genInvalid = elements [read "NaN", read "Infinity"]

instance GenUnchecked Double where
    genUnchecked = arbitrary

instance GenValid Double

-- | Either 'NaN' or 'Infinity'.
instance GenInvalid Double where
    genInvalid = elements [read "NaN", read "Infinity"]

instance GenUnchecked Integer where
    genUnchecked = arbitrary

instance GenValid Integer

-- | 'upTo' generates an integer between 0 (inclusive) and 'n'.
upTo :: Int -> Gen Int
upTo n
    | n <= 0 = pure 0
    | otherwise = elements [0 .. n]

-- | 'genSplit a' generates a tuple '(b, c)' such that 'b + c' equals 'a'.
genSplit :: Int -> Gen (Int, Int)
genSplit n
    | n < 0 = pure (0, 0)
    | otherwise = elements [(i, n - i) | i <- [0 .. n]]

-- | 'genSplit a' generates a triple '(b, c, d)' such that 'b + c + d' equals 'a'.
genSplit3 :: Int -> Gen (Int, Int, Int)
genSplit3 n
    | n < 0 = pure (0, 0, 0)
    | otherwise = do
        (a, z) <- genSplit n
        (b, c) <- genSplit z
        return (a, b, c)

-- | 'arbPartition n' generates a list 'ls' such that 'sum ls' equals 'n'.
arbPartition :: Int -> Gen [Int]
arbPartition k
    | k <= 0 = pure []
    | otherwise = do
        first <- elements [1 .. k]
        rest <- arbPartition $ k - first
        return $ first : rest

-- | A version of @listOf@ that takes size into account more accurately.
genListOf :: Gen a -> Gen [a]
genListOf func =
    sized $ \n -> do
        size <- upTo n
        pars <- arbPartition size
        forM pars $ \i -> resize i func
