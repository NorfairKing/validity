module Data.Validity.Set where

import Data.Validity

import Data.Set (Set)
import qualified Data.Set as S

-- | A 'Set' of things is valid if all the elements are valid and the 'Set' itself
-- is valid.
instance (Ord v,Validity v) =>
         Validity (Set v) where
    isValid s = S.valid s && all isValid s
