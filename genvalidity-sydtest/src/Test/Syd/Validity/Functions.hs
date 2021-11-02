{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Validity tests involving functions
module Test.Syd.Validity.Functions
  ( module Test.Syd.Validity.Functions.CanFail,
    module Test.Syd.Validity.Functions.Equivalence,
    module Test.Syd.Validity.Functions.Idempotence,
    module Test.Syd.Validity.Functions.Inverse,
    module Test.Syd.Validity.Functions.Validity,
  )
where

import Test.Syd.Validity.Functions.CanFail
import Test.Syd.Validity.Functions.Equivalence
import Test.Syd.Validity.Functions.Idempotence
import Test.Syd.Validity.Functions.Inverse
import Test.Syd.Validity.Functions.Validity
