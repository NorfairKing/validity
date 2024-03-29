{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Validity.Scientific where

import Data.Scientific
import Data.Validity

-- | A 'Scientific' is valid according to the validity of its coefficient and exponent.
instance Validity Scientific where
  validate s =
    mconcat
      [ annotate (coefficient s) "coefficient",
        annotate (base10Exponent s) "base10Exponent"
      ]
