{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.DeepSeq
import Criterion.Main
import Data.GenValidity
import Data.GenValidity.Criterion
import Data.GenValidity.Persist ()
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics (Generic)

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

instance NFData (Key Thing) where
  rnf = rnf . fromSqlKey

instance NFData Thing

instance NFData (Entity Thing) where
  rnf (Entity k v) = seq (rnf k) $ rnf v

main :: IO ()
main =
  defaultMain
    [ genValidBench @ThingId,
      genValidBench @(Entity Thing)
    ]
