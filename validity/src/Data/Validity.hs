{-# LANGUAGE FlexibleInstances #-}
module Data.Validity
    ( Validity(..)
    ) where


-- | A class of types that have additional invariants defined upon them
-- that aren't enforced by the type system
class Validity a where
    isValid :: a -> Bool

-- | Any @Foldable@ of things that can be checked for validity can be checked
-- for validity
--
-- This includes lists, which means that the empty list is considered valid.
-- If the empty list should not be considered valid as part of your custom data
-- type, make sure to write a custom @Validity instance@
--
-- This also includes @Maybe@:
-- It makes sense to assume that 'Nothing' is valid.
-- If Nothing wasn't valid, you wouldn't have used a Maybe
-- in the datastructure.
instance (Validity a, Foldable t) => Validity (t a) where
    isValid = all isValid

