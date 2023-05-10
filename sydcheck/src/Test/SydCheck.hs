module Test.SydCheck
  ( -- * Properties
    TypedProperty,
    TypedPropertyT (..),
    IsTypedPropertyT,

    -- ** Constructing properties
    forAll,
    forAllValid,

    -- * Generator typeclass
    GenValid (..),

    -- ** Implementing 'GenValid'
    genValidStructurallyWithoutExtraChecking,

    -- * Generators
    Gen (..),
    Size (..),
    Seed (..),
    Randomness (..),
  )
where

import Test.SydCheck.Gen
import Test.SydCheck.GenValid
import Test.SydCheck.PList
import Test.SydCheck.Property
import Test.SydCheck.Randomness
import Test.SydCheck.Runner
import Test.SydCheck.Shrinking
