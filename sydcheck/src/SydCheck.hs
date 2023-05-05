module SydCheck
  ( -- * Properties
    TypedProperty (..),
    IsTypedProperty,

    -- ** Constructing properties
    forAll,
    forAllValid,

    -- * Generator typeclass
    GenValid (..),

    -- ** Implementing 'GenValid'
    genValidStructurallyWithoutExtraChecking,

    -- * Generators
    Gen (..),
  )
where

import SydCheck.Gen
import SydCheck.GenValid
import SydCheck.PList
import SydCheck.Property
import SydCheck.Randomness
import SydCheck.Runner
import SydCheck.Shrinking
