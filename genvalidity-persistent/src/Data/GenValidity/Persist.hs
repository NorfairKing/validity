{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Persist where

import Data.GenValidity
import Data.Validity.Persist ()
import Database.Persist
import Database.Persist.Sql

instance ToBackendKey SqlBackend record => GenUnchecked (Key record) where
  genUnchecked = toSqlKey <$> genUnchecked
  shrinkUnchecked = fmap toSqlKey . shrinkValid . fromSqlKey

instance ToBackendKey SqlBackend record => GenValid (Key record) where
  genValid = toSqlKey <$> genValid
  shrinkValid = shrinkUnchecked

instance
  (GenUnchecked a, PersistEntity a, ToBackendKey SqlBackend a) =>
  GenUnchecked (Entity a)
  where
  genUnchecked = Entity <$> genUnchecked <*> genUnchecked
  shrinkUnchecked (Entity k v) = [Entity k' v' | (k', v') <- shrinkUnchecked (k, v)]

instance (GenValid a, PersistEntity a, ToBackendKey SqlBackend a) => GenValid (Entity a) where
  genValid = Entity <$> genValid <*> genValid
  shrinkValid (Entity k v) = [Entity k' v' | (k', v') <- shrinkValid (k, v)]
