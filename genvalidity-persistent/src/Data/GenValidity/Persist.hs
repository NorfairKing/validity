{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Persist where

import Data.GenValidity
import Data.GenValidity.Containers
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Validity.Containers
import Data.Validity.Persist ()
import Database.Persist
import Database.Persist.Sql
import Test.QuickCheck

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

validsWithSeperateIDs ::
  forall a.
  (PersistEntity a, ToBackendKey SqlBackend a, GenValid a, Eq (Key a)) =>
  Gen [Entity a]
validsWithSeperateIDs = genValidsWithSeperateIDs genValid

genValidsWithSeperateIDs ::
  forall a.
  (PersistEntity a, ToBackendKey SqlBackend a, GenValid a, Eq (Key a)) =>
  Gen a ->
  Gen [Entity a]
genValidsWithSeperateIDs gen =
  sized $ \n -> do
    list <- arbPartition n
    go list
  where
    go :: [Int] -> Gen [Entity a]
    go [] = pure []
    go (s : ss) = do
      es <- go ss
      resize s $ do
        ei <- genValid `suchThat` (`notElem` map entityKey es)
        e <- gen
        pure $ Entity ei e : es

genSeperateIdsForNE ::
  forall a.
  (PersistEntity a, ToBackendKey SqlBackend a, GenValid a, Eq (Key a)) =>
  NonEmpty a ->
  Gen (NonEmpty (Entity a))
genSeperateIdsForNE (a :| as) = do
  es <- genSeperateIdsFor as
  i <- genValid `suchThat` (`notElem` map entityKey es)
  pure (Entity i a :| es)

genSeperateIds ::
  forall a.
  (PersistEntity a, ToBackendKey SqlBackend a, GenValid a, Eq (Key a)) =>
  Gen [Key a]
genSeperateIds = genSeperate genValid

genSeperateIdsFor ::
  forall a.
  (PersistEntity a, ToBackendKey SqlBackend a, GenValid a, Eq (Key a)) =>
  [a] ->
  Gen [Entity a]
genSeperateIdsFor [] = pure []
genSeperateIdsFor (a : as) = NE.toList <$> genSeperateIdsForNE (a :| as)

#if MIN_VERSION_containers(0,6,0)
shrinkValidWithSeperateIds ::
  (PersistEntity a, ToBackendKey SqlBackend a, GenValid a, Eq (Key a)) =>
  [Entity a] ->
  [[Entity a]]
shrinkValidWithSeperateIds = filter (distinctOrd . map entityKey) . shrinkValid
#endif
