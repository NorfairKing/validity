{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module SydCheck.PList where

import Data.Kind

data family PList (l :: [Type])

data instance PList '[] = PNil

data instance PList (x ': xs) = x `PCons` PList xs

deriving instance Show (PList '[])

deriving instance (Show x, Show (PList xs)) => Show (PList (x ': xs))

deriving instance Eq (PList '[])

deriving instance (Eq x, Eq (PList xs)) => Eq (PList (x ': xs))

deriving instance Ord (PList '[])

deriving instance (Ord x, Ord (PList xs)) => Ord (PList (x ': xs))
