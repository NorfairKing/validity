{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Syd.SydCheck
  ( -- * Typeclass specifications
    eqSpec,
    ordSpec,
    showReadSpec,

    -- * Function specifications
    shouldBeValid,
    producesValid,
    producesValid2,

    -- * Relation specifications

    -- ** Symmetry
    symmetry,
    symmetryOnGens,

    -- ** Reflexivity
    reflexivity,
    reflexivityOnGen,

    -- ** Transitivity
    transitivity,
    transitivityOnGens,

    -- ** Antisymmetry
    antisymmetry,
    antisymmetryOnGens,

    -- ** Antisymmetry
    antireflexivity,
    antireflexivityOnGen,

    -- ** Utils
    nameOf,

    -- ** Reexport SydCheck
    module SydCheck,

    -- ** Internal
    runSydCheckPropertyWithArgs,
  )
where

import Data.Typeable
import SydCheck
import Test.Syd
import Test.Syd.SydCheck.Eq
import Test.Syd.SydCheck.Relations
import Test.Syd.SydCheck.Runner
import Test.Syd.SydCheck.Utils

ordSpec :: forall a. Spec
ordSpec = pure ()

showReadSpec :: forall a. Spec
showReadSpec = pure ()

shouldBeValid :: a -> IO ()
shouldBeValid = undefined

producesValid :: (a -> b) -> TypedProperty '[a]
producesValid = undefined

producesValid2 :: (a -> b -> c) -> TypedProperty '[a, b]
producesValid2 = undefined
