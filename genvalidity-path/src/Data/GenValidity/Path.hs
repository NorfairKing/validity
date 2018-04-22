{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

module Data.GenValidity.Path where
#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
#endif
import Data.GenValidity
import Data.Validity.Path ()

import Path
import Path.Internal

import Test.QuickCheck.Gen

instance GenUnchecked (Path a f) where
    genUnchecked = Path <$> genUnchecked
    shrinkUnchecked (Path s) = Path <$> shrinkUnchecked s

instance GenValid (Path Abs File) where
    genValid = (Path . ('/' :) <$> genUnchecked) `suchThat` isValid

instance GenValid (Path Abs Dir) where
    genValid = (Path . ('/' :) . (++ "/") <$> genUnchecked) `suchThat` isValid

instance GenValid (Path Rel File) where
    genValid = (Path <$> genUnchecked) `suchThat` isValid

instance GenValid (Path Rel Dir) where
    genValid = (Path . (++ "/") <$> genUnchecked) `suchThat` isValid

instance GenInvalid (Path Abs File)

instance GenInvalid (Path Abs Dir)

instance GenInvalid (Path Rel File)

instance GenInvalid (Path Rel Dir)
