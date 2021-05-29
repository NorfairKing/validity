{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Properties of relations
module Test.Syd.Validity.Relations
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

import Test.Syd.Validity.Relations.Antireflexivity
import Test.Syd.Validity.Relations.Antisymmetry
import Test.Syd.Validity.Relations.Reflexivity
import Test.Syd.Validity.Relations.Symmetry
import Test.Syd.Validity.Relations.Transitivity
