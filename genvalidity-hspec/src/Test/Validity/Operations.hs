{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Properties of operations
module Test.Validity.Operations
    ( leftIdentityOnElemWithEquality
    , leftIdentityOnGenWithEquality
    , leftIdentityOnGen
    , leftIdentityOnValid
    , leftIdentity
    , leftIdentityOnArbitrary
    , rightIdentityOnElemWithEquality
    , rightIdentityOnGenWithEquality
    , rightIdentityOnGen
    , rightIdentityOnValid
    , rightIdentity
    , rightIdentityOnArbitrary
    , identityOnGen
    , identityOnValid
    , identity
    , identityOnArbitrary
    , associativeOnGens
    , associativeOnValids
    , associative
    , associativeOnArbitrary
    , commutativeOnGens
    , commutativeOnValids
    , commutative
    , commutativeOnArbitrary
    ) where

import Test.Validity.Operations.Associativity
import Test.Validity.Operations.Commutativity
import Test.Validity.Operations.Identity
