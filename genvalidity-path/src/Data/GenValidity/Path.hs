{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Path where

import Data.GenValidity
import Data.Validity.Path ()
import Path
import Path.Internal
import Test.QuickCheck.Gen

instance GenValid (Path Abs File) where
  genValid = (Path . ('/' :) <$> genValid) `suchThat` isValid
  shrinkValid (Path s) = filter isValid $ Path <$> shrinkValid s

instance GenValid (Path Abs Dir) where
  genValid = (Path . ('/' :) . (++ "/") <$> genValid) `suchThat` isValid
  shrinkValid (Path s) = filter isValid $ Path <$> shrinkValid s

instance GenValid (Path Rel File) where
  genValid = (Path <$> genValid) `suchThat` isValid
  shrinkValid (Path s) = filter isValid $ Path <$> shrinkValid s

instance GenValid (Path Rel Dir) where
  genValid = (Path . (++ "/") <$> genValid) `suchThat` isValid
  shrinkValid (Path s) = filter isValid $ Path <$> shrinkValid s
