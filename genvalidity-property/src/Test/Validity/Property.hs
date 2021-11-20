{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Validity.Property
  ( module Data.GenValidity,
    forAllValid,

    -- * Tests for GenValidity instances
    genGeneratesValid,

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
  )
where

import Data.GenValidity
import Test.Validity.Functions
import Test.Validity.GenValidity.Property
import Test.Validity.Operations
import Test.Validity.Property.Utils
import Test.Validity.Relations
import Test.Validity.Types
