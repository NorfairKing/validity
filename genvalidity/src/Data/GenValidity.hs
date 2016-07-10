module Data.GenValidity where

import           Data.Validity

import           Test.QuickCheck

-- | A class of types for which @Validity@-related values can be generated.
--
-- If you also write @Arbitrary@ instances for @GenValidity@ types, it may be
-- best to simply write @arbitrary = genValid@.
class Validity a => GenValidity a where
    -- | Generate a truly arbitrary datum, this should cover all possible
    -- values in the type
    genUnchecked :: Gen a

    -- | Generate a valid datum, this should cover all possible valid values in
    -- the type
    genValid :: Gen a
    genValid = genUnchecked `suchThat` isValid

    -- | Generate an invalid datum, this should cover all possible invalid
	-- values
    genInvalid :: Gen a
    genInvalid = genUnchecked `suchThat` (not . isValid)
    {-# MINIMAL genUnchecked #-}


instance GenValidity a => GenValidity (Maybe a) where
    genUnchecked = oneof [pure Nothing, Just <$> genUnchecked]
    genValid     = oneof [pure Nothing, Just <$> genValid]
    genInvalid   = Just <$> genInvalid


instance GenValidity a => GenValidity [a] where
    genUnchecked = genListOf genUnchecked

    genValid     = genListOf genValid

    -- | At least one invalid value in the list, the rest could be either.
    genInvalid   = do
        before <- genListOf genUnchecked
        middle <- genInvalid
        after  <- genListOf genUnchecked
        return $ before ++ [middle] ++ after

-- | A version of @listOf@ that takes size into account more accurately.
genListOf :: Gen a -> Gen [a]
genListOf func = sized $ \n ->
    case n of
        0 -> pure []
        m -> do
            pars <- arbPartition m
            forM pars $ \i -> resize i func
  where
    arbPartition :: Int -> Gen [Int]
    arbPartition 0 = pure []
    arbPartition 1 = pure [1]
    arbPartition k = do
        first <- elements [1..k]
        rest <- arbPartition $ k - first
        return $ first : rest
