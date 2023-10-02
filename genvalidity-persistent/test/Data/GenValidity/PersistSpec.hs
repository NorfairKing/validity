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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.GenValidity.PersistSpec
  ( spec,
  )
where

import Data.GenValidity.Persist
import Data.Validity.Containers
import Database.Persist
import Database.Persist.TH
import GHC.Generics (Generic)
import Test.Hspec
import Test.QuickCheck
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

instance GenValid Thing

spec :: Spec
spec = do
  genValidSpec @ThingId
  shrinkValidSpec @ThingId
  genValidSpec @(Entity Thing)
  shrinkValidSpec @(Entity Thing)
  describe "genSeperateIds" $
    it "generates values with seperate ids" $
      forAll genSeperateIds $
        \is -> distinctOrd (is :: [Key Thing])
  describe "genSeperateIdsFor" $
    it "generates values with seperate ids" $
      forAll genValid $
        \as ->
          forAll (genSeperateIdsFor as) $ \es -> distinctOrd $ map entityKey (es :: [Entity Thing])
  describe "genValidsWithSeperateIds" $ do
    it "generates values with seperate ids" $
      forAll (genValidsWithSeperateIDs genValid) $
        \es ->
          distinctOrd $ map entityKey (es :: [Entity Thing])
    it "generates values with seperate ids" $
      forAll (genValidsWithSeperateIDs genValid) $
        \es ->
          distinctOrd $ map entityKey (es :: [Entity Thing])
  describe "validsWithSeperateIDs" $
    it "generates values with seperate ids" $
      forAll validsWithSeperateIDs $
        \es -> distinctOrd $ map entityKey (es :: [Entity Thing])
