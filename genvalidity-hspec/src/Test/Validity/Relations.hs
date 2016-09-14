{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.Relations
    (
    -- * Properties of relations

    -- ** Reflexivity
      reflexivityOnGen
    , reflexivityOnValid
    , reflexivity
    , reflexivityOnArbitrary

    -- ** Transitivity
    , transitivityOnGens
    , transitivityOnValid
    , transitivity
    , transitivityOnArbitrary

    -- ** Antisymmetry
    , antisymmetryOnGensWithEquality
    , antisymmetryOnGensEq
    , antisymmetryOnValid
    , antisymmetry
    , antisymmetryOnArbitrary

    -- ** Symmetry
    , symmetryOnGens
    , symmetryOnValid
    , symmetry
    , symmetryOnArbitrary
    ) where

import           Test.Validity.Relations.Reflexivity
import           Test.Validity.Relations.Antisymmetry
import           Test.Validity.Relations.Symmetry
import           Test.Validity.Relations.Transitivity

