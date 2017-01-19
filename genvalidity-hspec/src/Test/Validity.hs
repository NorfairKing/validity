{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | To use the 'Spec' functions in this module, you will need @TypeApplications@.
module Test.Validity
    ( module Data.GenValidity
      -- * Tests for Arbitrary instances involving Validity
    , arbitrarySpec
    , arbitraryGeneratesOnlyValid
      -- * Tests for GenValidity instances
    , genValiditySpec
    , genValidSpec
    , genInvalidSpec
    , genValidGeneratesValid
    , genGeneratesValid
    , genInvalidGeneratesInvalid
    , genGeneratesInvalid
      -- * Tests for RelativeValidity instances
    , relativeValiditySpec
    , relativeValidityImpliesValidA
    , relativeValidityImpliesValidB
      -- * Tests for GenRelativeValidity instances
    , genRelativeValiditySpec
    , genRelativeValidGeneratesValid
    , genRelativeInvalidGeneratesInvalid
      -- * Standard tests involving functions
      -- ** Standard tests involving validity
    , producesValidsOnGen
    , producesValidsOnValids
    , producesValid
    , producesValidsOnArbitrary
    , producesValidsOnGens2
    , producesValidsOnValids2
    , producesValid2
    , producesValidsOnArbitrary2
    , producesValidsOnGens3
    , producesValidsOnValids3
    , producesValid3
    , producesValidsOnArbitrary3
      -- ** Standard tests involving functions that can fail
    , CanFail(..)
    , succeedsOnGen
    , succeedsOnValid
    , succeeds
    , succeedsOnArbitrary
    , succeedsOnGens2
    , succeedsOnValids2
    , succeeds2
    , succeedsOnArbitrary2
    , failsOnGen
    , failsOnInvalid
    , failsOnGens2
    , failsOnInvalid2
    , validIfSucceedsOnGen
    , validIfSucceedsOnValid
    , validIfSucceedsOnArbitrary
    , validIfSucceeds
    , validIfSucceedsOnGens2
    , validIfSucceedsOnValids2
    , validIfSucceeds2
    , validIfSucceedsOnArbitrary2
      -- ** Standard tests involving equivalence of functions
      -- *** Simple functions
      -- **** One argument
    , equivalentOnGen
    , equivalentOnValid
    , equivalent
    , equivalentOnArbitrary
      -- **** Two arguments
    , equivalentOnGens2
    , equivalentOnValids2
    , equivalent2
    , equivalentOnArbitrary2
      -- **** Three arguments
    , equivalentOnGens3
    , equivalentOnValids3
    , equivalent3
    , equivalentOnArbitrary3
      -- *** First function can fail
      -- **** One argument
    , equivalentWhenFirstSucceedsOnGen
    , equivalentWhenFirstSucceedsOnValid
    , equivalentWhenFirstSucceeds
    , equivalentWhenFirstSucceedsOnArbitrary
      -- **** Two arguments
    , equivalentWhenFirstSucceedsOnGens2
    , equivalentWhenFirstSucceedsOnValids2
    , equivalentWhenFirstSucceeds2
    , equivalentWhenFirstSucceedsOnArbitrary2
      -- *** Second function can fail
      -- **** One argument
    , equivalentWhenSecondSucceedsOnGen
    , equivalentWhenSecondSucceedsOnValid
    , equivalentWhenSecondSucceeds
    , equivalentWhenSecondSucceedsOnArbitrary
      -- **** Two arguments
    , equivalentWhenSecondSucceedsOnGens2
    , equivalentWhenSecondSucceedsOnValids2
    , equivalentWhenSecondSucceeds2
    , equivalentWhenSecondSucceedsOnArbitrary2
      -- *** Both functions can fail
      -- **** One argument
    , equivalentWhenSucceedOnGen
    , equivalentWhenSucceedOnValid
    , equivalentWhenSucceed
    , equivalentWhenSucceedOnArbitrary
      -- **** Two arguments
    , equivalentWhenSucceedOnGens2
    , equivalentWhenSucceedOnValids2
    , equivalentWhenSucceed2
    , equivalentWhenSucceedOnArbitrary2
      -- ** Standard tests involving inverse functions
    , inverseFunctionsOnGen
    , inverseFunctionsOnValid
    , inverseFunctions
    , inverseFunctionsOnArbitrary
    , inverseFunctionsIfFirstSucceedsOnGen
    , inverseFunctionsIfFirstSucceedsOnValid
    , inverseFunctionsIfFirstSucceeds
    , inverseFunctionsIfFirstSucceedsOnArbitrary
    , inverseFunctionsIfSecondSucceedsOnGen
    , inverseFunctionsIfSecondSucceedsOnValid
    , inverseFunctionsIfSecondSucceeds
    , inverseFunctionsIfSecondSucceedsOnArbitrary
    , inverseFunctionsIfSucceedOnGen
    , inverseFunctionsIfSucceedOnValid
    , inverseFunctionsIfSucceed
    , inverseFunctionsIfSucceedOnArbitrary
      -- ** Properties involving idempotence
    , idempotentOnGen
    , idempotentOnValid
    , idempotent
    , idempotentOnArbitrary
      -- * Properties of relations
      -- ** Reflexivity
    , reflexiveOnElem
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
    , antisymmetryOnGens
    , antisymmetryOnValid
    , antisymmetry
    , antisymmetryOnArbitrary
      -- ** Antireflexivity
    , antireflexiveOnElem
    , antireflexivityOnGen
    , antireflexivityOnValid
    , antireflexivity
    , antireflexivityOnArbitrary
      -- ** Symmetry
    , symmetricOnElems
    , symmetryOnGens
    , symmetryOnValid
    , symmetry
    , symmetryOnArbitrary
      -- * Properties of operations
      -- ** Identity element
      -- *** Left Identity
    , leftIdentityOnElemWithEquality
    , leftIdentityOnGenWithEquality
    , leftIdentityOnGen
    , leftIdentityOnValid
    , leftIdentity
    , leftIdentityOnArbitrary
      -- *** Right Identity
    , rightIdentityOnElemWithEquality
    , rightIdentityOnGenWithEquality
    , rightIdentityOnGen
    , rightIdentityOnValid
    , rightIdentity
    , rightIdentityOnArbitrary
      -- *** Identity
    , identityOnGen
    , identityOnValid
    , identity
    , identityOnArbitrary
      -- ** Associativity
    , associativeOnGens
    , associativeOnValids
    , associative
    , associativeOnArbitrary
      -- ** Commutativity
    , commutativeOnGens
    , commutativeOnValids
    , commutative
    , commutativeOnArbitrary
      -- * Eq properties
    , eqSpecOnValid
    , eqSpec
    , eqSpecOnArbitrary
    , eqSpecOnGen
      -- * Ord properties
    , ordSpecOnGen
    , ordSpecOnValid
    , ordSpec
    , ordSpecOnArbitrary
      -- * Monoid properties
    , monoidSpecOnValid
    , monoidSpec
    , monoidSpecOnArbitrary
    , monoidSpecOnGen
      -- * Functor properties
    , functorSpecOnValid
    , functorSpec
    , functorSpecOnArbitrary
    , functorSpecOnGens
      -- * Applicative properties
    , applicativeSpecOnValid
    , applicativeSpec
    , applicativeSpecOnArbitrary
    , applicativeSpecOnGens
      -- * Monad properties
    , monadSpecOnValid
    , monadSpec
    , monadSpecOnArbitrary
    , monadSpecOnGens
    ) where

import Data.Data
import Data.GenValidity

import Test.Validity.Arbitrary
import Test.Validity.Eq
import Test.Validity.Functions
import Test.Validity.Functor
import Test.Validity.Applicative
import Test.Validity.Monad
import Test.Validity.GenRelativeValidity
import Test.Validity.GenValidity
import Test.Validity.Monoid
import Test.Validity.Operations
import Test.Validity.Ord
import Test.Validity.Relations
import Test.Validity.RelativeValidity
import Test.Validity.Types
