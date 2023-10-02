{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Utilities for defining your own validity 'Spec's
--
-- You will need @TypeApplications@ to use these.
module Test.Validity.Utils
  ( nameOf,
    genDescr,
    binRelStr,
    shouldFail,
    failsBecause,
    Anon (..),
    shouldBeValid,
    shouldBeInvalid,
  )
where

import Control.Arrow (second)
import Control.Monad.Trans.Writer (mapWriterT)
import Data.Data
import Test.Hspec
import Test.Hspec.Core.Formatters
import Test.Hspec.Core.Runner
import Test.Hspec.Core.Spec
import Test.QuickCheck.Property
import Test.Validity.Property.Utils

nameOf ::
  forall a.
  Typeable a =>
  String
nameOf =
  let s = show $ typeRep (Proxy @a)
   in if ' ' `elem` s
        then "(" ++ s ++ ")"
        else s

genDescr ::
  forall a.
  Typeable a =>
  String ->
  String
genDescr genname = unwords ["\"" ++ genname, "::", nameOf @a ++ "\""]

binRelStr ::
  forall a.
  Typeable a =>
  String ->
  String
binRelStr op = unwords ["(" ++ op ++ ")", "::", name, "->", name, "->", "Bool"]
  where
    name = nameOf @a

newtype Anon a
  = Anon a

instance Show (Anon a) where
  show _ = "Anonymous"

instance Functor Anon where
  fmap f (Anon a) = Anon (f a)

-- I'm not sure why mapSpecTree was removed from hspec-core,
-- but it has been copied here for convenience.
-- https://github.com/hspec/hspec/commit/020c7ecc4a73c24af38e9fab049f60bb9aec6981#diff-29cb22f0ef6e98086a71fc045847bd21L22
mapSpecTree' :: (SpecTree a -> SpecTree b) -> SpecM a r -> SpecM b r

#if MIN_VERSION_hspec(2,10,0)
mapSpecTree' f (SpecM specs) = SpecM (mapWriterT (fmap (second (fmap (map f)))) specs)
#else
mapSpecTree' f (SpecM specs) = SpecM (mapWriterT (fmap (second (map f))) specs)
#endif

-- | Asserts that a given 'Spec' tree fails _somewhere_.
--
-- It also shows the given string when reporting that the tree unexpectedly
-- succeeded.
failsBecause :: String -> SpecWith () -> SpecWith ()
failsBecause s = mapSpecTree' go
  where
    go :: SpecTree () -> SpecTree ()
    go sp =
      Leaf
        Item
          { itemRequirement = s,
            itemLocation = Nothing,
            itemIsFocused = False,
            itemIsParallelizable = Nothing,
            itemExample =
              \_ _ _ -> do
                let conf =
                      defaultConfig {configFormatter = Just silent}
                r <- hspecWithResult conf $ fromSpecList [sp]
                let succesful =
                      summaryExamples r > 0 && summaryFailures r > 0
                pure $ produceResult succesful
          }

produceResult :: Bool -> Test.Hspec.Core.Spec.Result
produceResult succesful =
  Result
    { resultInfo = "",
      resultStatus =
        if succesful
          then Success
          else Failure Nothing $ Test.Hspec.Core.Spec.Reason "Should have failed but didn't."
    }

shouldFail :: Property -> Property
shouldFail =
  mapResult $ \res ->
    res
      { reason = unwords ["Should have failed:", reason res],
        expect = not $ expect res
      }
