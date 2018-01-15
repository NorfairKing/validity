{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-| Relative validity
    -}
module Data.RelativeValidity
    ( RelativeValidity(..)
    , isInvalidFor
    ) where

-- | A class of types that have additional invariants defined upon them
-- that aren't enforced by the type system
--
-- If there is a @Validity a@ instance as well, then @a `isValidFor` b@
-- should imply @isValid a@ for any @b@.
--
-- If there is a @Validity b@ instance as well, then @a `isValidFor` b@
-- should imply @isValid b@ for any @a@.
class RelativeValidity a b where
    isValidFor :: a -> b -> Bool

isInvalidFor :: RelativeValidity a b => a -> b -> Bool
isInvalidFor a b = not $ isValidFor a b
