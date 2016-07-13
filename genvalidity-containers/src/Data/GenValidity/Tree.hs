module Data.GenValidity.Tree where

import           Data.GenValidity

import           Test.QuickCheck

import           Data.Tree

instance GenValidity a => GenValidity (Tree a) where
    genUnchecked = genTreeOf genUnchecked

    genValid     = genTreeOf genValid

    -- | There should be at least one invalid element, either it's here or it's
    -- further down the tree.
    genInvalid   = oneof
        [ Node <$> genInvalid   <*> genUnchecked
        , Node <$> genUnchecked <*> genInvalid
        ]

-- | Generate a tree of values that are generated as specified.
--
-- This takes the size parameter much better into account
genTreeOf :: Gen a -> Gen (Tree a)
genTreeOf func = sized $ \n -> -- Sized is the size of the trees.
    case n of
        0 -> Node <$> func <*> pure []
        1 -> Node <$> func <*> pure []
        m -> do
            value <- func
            forest <- resize (m - 1) $ genListOf $ genTreeOf func
            return $ Node value forest

