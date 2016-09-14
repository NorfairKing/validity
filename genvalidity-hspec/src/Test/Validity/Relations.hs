{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Test.Validity.Relations
    (
    -- * Properties of relations

    -- ** Reflexivity
      reflexiveOnElem
    , reflexivityOnGen
    , reflexivityOnValid
    , reflexivity
    , reflexivityOnArbitrary

    -- ** Transitivity
    , transitiveOnElems
    , transitivityOnGens
    , transitivityOnValid
    , transitivity
    , transitivityOnArbitrary

    -- ** Antisymmetry
    , antisymmetricOnElemsWithEquality
    , antisymmetryOnGensWithEquality
    , antisymmetryOnGensEq
    , antisymmetryOnValid
    , antisymmetry
    , antisymmetryOnArbitrary

    -- ** Symmetry
    , symmetricOnElems
    , symmetryOnGens
    , symmetryOnValid
    , symmetry
    , symmetryOnArbitrary
    ) where

import           Test.Validity.Relations.Reflexivity
import           Test.Validity.Relations.Antisymmetry
import           Test.Validity.Relations.Symmetry
import           Test.Validity.Relations.Transitivity

