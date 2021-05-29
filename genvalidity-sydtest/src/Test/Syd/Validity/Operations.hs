{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Properties of operations
module Test.Syd.Validity.Operations
  ( leftIdentityOnElemWithEquality,
    leftIdentityOnGenWithEquality,
    leftIdentityOnGen,
    leftIdentityOnValid,
    leftIdentity,
    leftIdentityOnArbitrary,
    rightIdentityOnElemWithEquality,
    rightIdentityOnGenWithEquality,
    rightIdentityOnGen,
    rightIdentityOnValid,
    rightIdentity,
    rightIdentityOnArbitrary,
    identityOnGen,
    identityOnValid,
    identity,
    identityOnArbitrary,
    associativeOnGens,
    associativeOnValids,
    associative,
    associativeOnArbitrary,
    commutativeOnGens,
    commutativeOnValids,
    commutative,
    commutativeOnArbitrary,
  )
where

import Test.Syd.Validity.Operations.Associativity
import Test.Syd.Validity.Operations.Commutativity
import Test.Syd.Validity.Operations.Identity
