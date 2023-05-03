{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module SydCheck.Property where

import SydCheck.Gen
import SydCheck.GenValid

data Property ls where
  PropBool :: Bool -> Property '[]
  PropGen :: Gen a -> (a -> Property ls) -> Property (a ': ls)

class IsProperty ls a | a -> ls where
  toProperty :: a -> Property ls

instance IsProperty ls (Property ls) where
  toProperty = id

instance IsProperty '[] Bool where
  toProperty = PropBool

instance (GenValid a, IsProperty ls b) => IsProperty (a ': ls) (a -> b) where
  toProperty func = forAll genValid $ \a -> func a

forAll :: IsProperty ls prop => Gen a -> (a -> prop) -> Property (a ': ls)
forAll gen func = PropGen gen $ \a -> toProperty (func a)

-- forAllShrink does not exist anymore, yay
