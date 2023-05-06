{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module SydCheck.PList where

import Data.Kind

-- | HList, but with a 'P' to avoid naming conflicts
data PList (ls :: [Type]) where
  PNil :: PList '[]
  PCons :: Show a => a -> PList ls -> PList (a ': ls)

instance Show (PList ls) where
  showsPrec d = \case
    PNil -> showString "PNil"
    PCons a ls ->
      showString "PCons "
        . showsPrec 10 a
        . showString " "
        . showsPrec d ls

instance Eq (PList '[]) where
  PNil == PNil = True

instance (Eq a, Eq (PList ls)) => Eq (PList (a ': ls)) where
  PCons a1 ls1 == PCons a2 ls2 = a1 == a2 && ls1 == ls2
