{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.GenValidity.PersistSpec
  ( spec,
  )
where

import Data.GenValidity.Persist ()
import Database.Persist
import Database.Persist.TH
import GHC.Generics (Generic)
import Test.Hspec
import Test.Validity

share
  [mkPersist sqlSettings]
  [persistLowerCase|

Thing
  number Int

  deriving Show
  deriving Eq
  deriving Ord
  deriving Generic

|]

instance Validity Thing

instance GenUnchecked Thing

instance GenValid Thing

spec :: Spec
spec = do
  genValidSpec @ThingId
  shrinkValidSpec @ThingId
  genValidSpec @(Entity Thing)
  shrinkValidSpec @(Entity Thing)
