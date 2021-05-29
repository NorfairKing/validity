{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Properties of relations
module Test.Validity.Relations
  ( reflexiveOnElem,
    reflexivityOnGen,
    reflexivityOnValid,
    reflexivity,
    reflexivityOnArbitrary,
    transitiveOnElems,
    transitivityOnGens,
    transitivityOnValid,
    transitivity,
    transitivityOnArbitrary,
    antisymmetricOnElemsWithEquality,
    antisymmetryOnGensWithEquality,
    antisymmetryOnGens,
    antisymmetryOnValid,
    antisymmetry,
    antisymmetryOnArbitrary,
    antireflexiveOnElem,
    antireflexivityOnGen,
    antireflexivityOnValid,
    antireflexivity,
    antireflexivityOnArbitrary,
    symmetricOnElems,
    symmetryOnGens,
    symmetryOnValid,
    symmetry,
    symmetryOnArbitrary,
  )
where

import Test.Validity.Relations.Antireflexivity
import Test.Validity.Relations.Antisymmetry
import Test.Validity.Relations.Reflexivity
import Test.Validity.Relations.Symmetry
import Test.Validity.Relations.Transitivity
