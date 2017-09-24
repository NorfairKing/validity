{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Scientific where

import Data.Validity

import Data.Scientific

-- | A 'Scientific' is valid according to the validity of its coefficient and exponent.
instance Validity Scientific where
    isValid s = isValid (coefficient s) && isValid (base10Exponent s)
    validate s =
        mconcat
            [ coefficient s <?!> "coefficient"
            , base10Exponent s <?!> "base10Exponent"
            ]
