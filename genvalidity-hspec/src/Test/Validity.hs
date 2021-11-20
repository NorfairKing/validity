{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | To use the 'Spec' functions in this module, you will need @TypeApplications@.
--
--
-- The most interesting functions in this module for most uses are:
--
-- * 'genValidSpec'
-- * 'eqSpec'
-- * 'ordSpec'
-- * 'producesValidsOnValids'
-- * 'forAllValid'
-- * 'shouldBeValid'
module Test.Validity
  ( -- * Writing properties

    -- ** Cheap generation with shrinking
    forAllValid,

    -- ** Cheap assertions
    shouldBeValid,
    shouldBeInvalid,

    -- * Tests for GenValidity instances
    genValidSpec,
    genValidGeneratesValid,
    genGeneratesValid,
    shrinkValidSpec,
    shrinkValidSpecWithLimit,
    shrinkValidPreservesValidOnGenValid,
    shrinkPreservesValidOnGenValid,
    shrinkValidPreservesValid,
    shrinkingStaysValid,
    shrinkingPreserves,

    -- * Tests for Arbitrary instances involving Validity
    arbitrarySpec,
    arbitraryGeneratesOnlyValid,

    -- * Standard tests involving functions

    -- ** Standard tests involving validity
    producesValidsOnGen,
    producesValid,
    producesValidsOnArbitrary,
    producesValidsOnGens2,
    producesValid2,
    producesValidsOnArbitrary2,
    producesValidsOnGens3,
    producesValid3,
    producesValidsOnArbitrary3,

    -- ** Standard tests involving functions that can fail
    CanFail (..),
    succeedsOnGen,
    succeeds,
    succeedsOnArbitrary,
    succeedsOnGens2,
    succeeds2,
    succeedsOnArbitrary2,
    failsOnGen,
    failsOnGens2,
    validIfSucceedsOnGen,
    validIfSucceedsOnArbitrary,
    validIfSucceeds,
    validIfSucceedsOnGens2,
    validIfSucceeds2,
    validIfSucceedsOnArbitrary2,
    validIfSucceedsOnGens3,
    validIfSucceeds3,
    validIfSucceedsOnArbitrary3,

    -- ** Standard tests involving equivalence of functions

    -- *** Simple functions

    -- **** One argument
    equivalentOnGen,
    equivalent,
    equivalentOnArbitrary,

    -- **** Two arguments
    equivalentOnGens2,
    equivalent2,
    equivalentOnArbitrary2,

    -- **** Three arguments
    equivalentOnGens3,
    equivalent3,
    equivalentOnArbitrary3,

    -- *** First function can fail

    -- **** One argument
    equivalentWhenFirstSucceedsOnGen,
    equivalentWhenFirstSucceeds,
    equivalentWhenFirstSucceedsOnArbitrary,

    -- **** Two arguments
    equivalentWhenFirstSucceedsOnGens2,
    equivalentWhenFirstSucceeds2,
    equivalentWhenFirstSucceedsOnArbitrary2,

    -- *** Second function can fail

    -- **** One argument
    equivalentWhenSecondSucceedsOnGen,
    equivalentWhenSecondSucceeds,
    equivalentWhenSecondSucceedsOnArbitrary,

    -- **** Two arguments
    equivalentWhenSecondSucceedsOnGens2,
    equivalentWhenSecondSucceeds2,
    equivalentWhenSecondSucceedsOnArbitrary2,

    -- *** Both functions can fail

    -- **** One argument
    equivalentWhenSucceedOnGen,
    equivalentWhenSucceed,
    equivalentWhenSucceedOnArbitrary,

    -- **** Two arguments
    equivalentWhenSucceedOnGens2,
    equivalentWhenSucceed2,
    equivalentWhenSucceedOnArbitrary2,

    -- ** Standard tests involving inverse functions
    inverseFunctionsOnGen,
    inverseFunctions,
    inverseFunctionsOnArbitrary,
    inverseFunctionsIfFirstSucceedsOnGen,
    inverseFunctionsIfFirstSucceeds,
    inverseFunctionsIfFirstSucceedsOnArbitrary,
    inverseFunctionsIfSecondSucceedsOnGen,
    inverseFunctionsIfSecondSucceeds,
    inverseFunctionsIfSecondSucceedsOnArbitrary,
    inverseFunctionsIfSucceedOnGen,
    inverseFunctionsIfSucceed,
    inverseFunctionsIfSucceedOnArbitrary,

    -- ** Properties involving idempotence
    idempotentOnGen,
    idempotent,
    idempotentOnArbitrary,

    -- * Properties of relations

    -- ** Reflexivity
    reflexiveOnElem,
    reflexivityOnGen,
    reflexivity,
    reflexivityOnArbitrary,

    -- ** Transitivity
    transitiveOnElems,
    transitivityOnGens,
    transitivity,
    transitivityOnArbitrary,

    -- ** Antisymmetry
    antisymmetricOnElemsWithEquality,
    antisymmetryOnGensWithEquality,
    antisymmetryOnGens,
    antisymmetry,
    antisymmetryOnArbitrary,

    -- ** Antireflexivity
    antireflexiveOnElem,
    antireflexivityOnGen,
    antireflexivity,
    antireflexivityOnArbitrary,

    -- ** Symmetry
    symmetricOnElems,
    symmetryOnGens,
    symmetry,
    symmetryOnArbitrary,

    -- * Properties of operations

    -- ** Identity element

    -- *** Left Identity
    leftIdentityOnElemWithEquality,
    leftIdentityOnGenWithEquality,
    leftIdentityOnGen,
    leftIdentity,
    leftIdentityOnArbitrary,

    -- *** Right Identity
    rightIdentityOnElemWithEquality,
    rightIdentityOnGenWithEquality,
    rightIdentityOnGen,
    rightIdentity,
    rightIdentityOnArbitrary,

    -- *** Identity
    identityOnGen,
    identity,
    identityOnArbitrary,

    -- ** Associativity
    associativeOnGens,
    associative,
    associativeOnArbitrary,

    -- ** Commutativity
    commutativeOnGens,
    commutative,
    commutativeOnArbitrary,

    -- * Show and Read properties
    showReadSpec,
    showReadSpecOnArbitrary,
    showReadSpecOnGen,

    -- * Eq properties
    eqSpec,
    eqSpecOnArbitrary,
    eqSpecOnGen,

    -- * Ord properties
    ordSpecOnGen,
    ordSpec,
    ordSpecOnArbitrary,

    -- * Monoid properties
    monoidSpec,
    monoidSpecOnArbitrary,
    monoidSpecOnGen,

    -- * Functor properties
    functorSpec,
    functorSpecOnArbitrary,
    functorSpecOnGens,

    -- * Applicative properties
    applicativeSpec,
    applicativeSpecOnArbitrary,
    applicativeSpecOnGens,

    -- * Monad properties
    monadSpec,
    monadSpecOnArbitrary,
    monadSpecOnGens,

    -- * Re-exports
    module Data.GenValidity,
  )
where

import Data.GenValidity
import Test.Validity.Applicative
import Test.Validity.Arbitrary
import Test.Validity.Eq
import Test.Validity.Functions
import Test.Validity.Functor
import Test.Validity.GenValidity
import Test.Validity.Monad
import Test.Validity.Monoid
import Test.Validity.Operations
import Test.Validity.Ord
import Test.Validity.Property
import Test.Validity.Show
import Test.Validity.Shrinking
import Test.Validity.Utils
