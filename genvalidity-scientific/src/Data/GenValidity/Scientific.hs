{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Scientific where

import Data.GenValidity
import Data.List
import Data.Scientific
import Data.Validity.Scientific ()

instance GenValid Scientific where
  genValid = scientific <$> genValid <*> genValid
  shrinkValid s =
    filter isValid $
      nub $
        filter (/= s) $
          [ scientific c e
            | (c, e) <- shrinkValid (coefficient s, base10Exponent s)
          ]
