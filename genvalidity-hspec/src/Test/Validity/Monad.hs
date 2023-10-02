{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Monad properties
--
-- You will need @TypeApplications@ to use these.
module Test.Validity.Monad
  ( monadSpec,
    monadSpecOnArbitrary,
    monadSpecOnGens,
  )
where

import Control.Monad (ap)
import Data.Data
import Data.GenValidity
import Data.Kind (Type)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)
import Test.Validity.Functions
import Test.Validity.Utils

{-# ANN module "HLint: ignore Use fmap" #-}

{-# ANN module "HLint: ignore Use <$>" #-}

{-# ANN module "HLint: ignore Use >=>" #-}

{-# ANN module "HLint: ignore Use id" #-}

{-# ANN module "HLint: ignore Monad law, left identity" #-}

{-# ANN module "HLint: ignore Monad law, right identity" #-}

{-# ANN module "HLint: ignore Avoid lambda" #-}

{-# ANN module "HLint: ignore Reduce duplication" #-}

returnTypeStr ::
  forall (m :: Type -> Type).
  (Typeable m) =>
  String
returnTypeStr = unwords ["return", "::", "a", "->", nameOf @m, "a"]

bindTypeStr ::
  forall (m :: Type -> Type).
  (Typeable m) =>
  String
bindTypeStr =
  unwords
    [ "(>>=)",
      "::",
      nameOf @m,
      "a",
      "->",
      "(b",
      "->",
      nameOf @m,
      "a)",
      "->",
      nameOf @m,
      "b"
    ]

-- | Standard test spec for properties of Monad instances for values generated with GenValid instances
--
-- Example usage:
--
-- > monadSpec @[]
monadSpec ::
  forall (f :: Type -> Type).
  (Eq (f Int), Show (f Int), Monad f, Typeable f, GenValid (f Int)) =>
  Spec
monadSpec = monadSpecWithInts @f genValid

-- | Standard test spec for properties of Monad instances for values generated with Arbitrary instances
--
-- Example usage:
--
-- > monadSpecOnArbitrary @[]
monadSpecOnArbitrary ::
  forall (f :: Type -> Type).
  (Eq (f Int), Show (f Int), Monad f, Typeable f, Arbitrary (f Int)) =>
  Spec
monadSpecOnArbitrary = monadSpecWithInts @f arbitrary

monadSpecWithInts ::
  forall (f :: Type -> Type).
  (Eq (f Int), Show (f Int), Monad f, Typeable f) =>
  Gen (f Int) ->
  Spec
monadSpecWithInts gen =
  monadSpecOnGens
    @f
    @Int
    genValid
    "int"
    gen
    (unwords [nameOf @f, "of ints"])
    gen
    (unwords [nameOf @f, "of ints"])
    ((+) <$> genValid)
    "increments"
    ( do
        s <- genListLength
        pure $ \b -> unGen gen (mkQCGen b) s
    )
    "perturbations using the int"
    ( do
        s <- genListLength
        pure $ \b -> unGen gen (mkQCGen $ 2 * b) s
    )
    "perturbations using the double the int"
    (pure <$> ((+) <$> genValid))
    (unwords [nameOf @f, "of additions"])

-- | Standard test spec for properties of Monad instances for values generated by given generators (and names for those generator).
--
-- Example usage:
--
-- > monadSpecOnGens
-- >     @[]
-- >     @Int
-- >     (pure 4)
-- >     "four"
-- >     (genListOf $ pure 5)
-- >     "list of fives"
-- >     (genListOf $ pure 6)
-- >     "list of sixes"
-- >     ((*) <$> genValid)
-- >     "factorisations"
-- >     (pure $ \a -> [a])
-- >     "singletonisation"
-- >     (pure $ \a -> [a])
-- >     "singletonisation"
-- >     (pure $ pure (+ 1))
-- >     "increment in list"
monadSpecOnGens ::
  forall (f :: Type -> Type) (a :: Type) (b :: Type) (c :: Type).
  ( Show a,
    Show (f a),
    Show (f b),
    Show (f c),
    Eq (f a),
    Eq (f b),
    Eq (f c),
    Monad f,
    Typeable f,
    Typeable a,
    Typeable b,
    Typeable c
  ) =>
  Gen a ->
  String ->
  Gen (f a) ->
  String ->
  Gen (f b) ->
  String ->
  Gen (a -> b) ->
  String ->
  Gen (a -> f b) ->
  String ->
  Gen (b -> f c) ->
  String ->
  Gen (f (a -> b)) ->
  String ->
  Spec
monadSpecOnGens gena genaname gen genname genb genbname geng gengname genbf genbfname gencf gencfname genfab genfabname =
  parallel $
    describe ("Monad " ++ nameOf @f) $ do
      describe (unwords [returnTypeStr @f, "and", bindTypeStr @f]) $ do
        it
          ( unwords
              [ "satisfy the first Monad law: 'return a >>= k = k a' for",
                genDescr @a genaname,
                "and",
                genDescr @(a -> f b) genbfname
              ]
          )
          $ equivalentOnGens2
            (\a (Anon k) -> return a >>= k)
            (\a (Anon k) -> k a)
            ((,) <$> gena <*> (Anon <$> genbf))
            shrinkNothing
        it
          ( unwords
              [ "satisfy the second Monad law: 'm >>= return = m' for",
                genDescr @(f a) genname
              ]
          )
          $ equivalentOnGen (\m -> m >>= return) (\m -> m) gen shrinkNothing
      describe (bindTypeStr @f)
        $ it
          ( unwords
              [ "satisfies the third Monad law: 'm >>= (x -> k x >>= h) = (m >>= k) >>= h' for",
                genDescr @(f a) genname,
                genDescr @(a -> f b) genbfname,
                "and",
                genDescr @(b -> f c) gencfname
              ]
          )
        $ equivalentOnGens3
          (\m (Anon k) (Anon h) -> m >>= (\x -> k x >>= h))
          (\m (Anon k) (Anon h) -> (m >>= k) >>= h)
          ((,,) <$> gen <*> (Anon <$> genbf) <*> (Anon <$> gencf))
          shrinkNothing
      describe (unwords ["relation with Applicative", nameOf @f]) $ do
        it
          ( unwords
              ["satisfies 'pure = return' for", genDescr @(f a) genname]
          )
          $ equivalentOnGen (pure @f) (return @f) gena shrinkNothing
        it
          ( unwords
              [ "satisfies '(<*>) = ap' for",
                genDescr @(f (a -> b)) genfabname,
                "and",
                genDescr @(f a) genname
              ]
          )
          $ equivalentOnGens2
            (\(Anon a) b -> a <*> b)
            (\(Anon a) b -> ap a b)
            ((,) <$> (Anon <$> genfab) <*> gen)
            shrinkNothing
        it
          ( unwords
              [ "satisfies '(>>) = (*>)' for",
                genDescr @(f a) genname,
                "and",
                genDescr @(f b) genbname
              ]
          )
          $ equivalentOnGens2 (>>) (*>) ((,) <$> gen <*> genb) shrinkNothing
      describe (unwords ["relation with Functor", nameOf @f])
        $ it
          ( unwords
              [ "satisfies 'fmap f xs = xs >>= return . f' for",
                genDescr @(a -> b) gengname,
                "and",
                genDescr @(f a) genname
              ]
          )
        $ equivalentOnGens2
          (\(Anon f) xs -> fmap f xs)
          (\(Anon f) xs -> xs >>= (return . f))
          ((,) <$> (Anon <$> geng) <*> gen)
          shrinkNothing
