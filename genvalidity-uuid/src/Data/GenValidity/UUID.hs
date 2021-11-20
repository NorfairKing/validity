{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.UUID where

import Data.GenValidity
import Data.UUID
import Data.Validity.UUID ()

instance GenValid UUID where
  genValid =
    fromWords <$> genValid <*> genValid <*> genValid
      <*> genValid
  shrinkValid u =
    [ fromWords w1 w2 w3 w4
      | (w1, w2, w3, w4) <- shrinkValid $ toWords u
    ]
