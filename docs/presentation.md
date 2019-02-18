---
title: Validity and Validity-based testing
author: Tom Sydney Kerckhove
---

# Invariants on data

``` haskell
module Prime (Prime, unPrime) where

-- INVARIANT: isPrime
newtype Prime = Prime { unPrime :: Int }
```


# Validity: Explicit Invariants

``` haskell
class Validity a where
  isValid :: a -> Bool

instance Validity Prime where
  isValid = isPrime . unPrime
```


## Semantics

* Invalid values should never appear in your program.
* `isValid` should be an underapproximation


# Validity: standard instances

``` haskell
instance Validity a => Validity [a] where
  isValid = all isValid

data MyType = { myPrime :: Prime, myText :: Text }
  deriving (Generic)

instance Validity MyType -- No boilerplate!
```


# Validity

``` haskell
λ isValid (Prime 6)
False

λ isValid [Prime 2, Prime 3, Prime 5, Prime 6]
False -- But why?
```

# Validity: Validate

``` haskell
class Validity a where
  validate :: a -> Validation -- Check _why_ it is valid
```


# Validity: Pretty Validate

``` 
λ prettyValidation [Prime 2, Prime 3, Prime 5, Prime 6]
The element at index 3 in the list
  \ Violated: Prime -- Aha!
```


# Testing: Unit test

```
myTest () {
  Run some code here.
  The test passes if this code does not crash.
  The test fails if this code crashes.
}
```

```
> myTest();
```


# Testing: Property test

```
myTest (argument) {
  Run some code here.
  The test passes if this code does not crash.
  The test fails if this code crashes.
}
```

```
> myTest(4);
```


# Testing: Random Property test

```
myTest (argument) {
  Run some code here.
  The test passes if this code does not crash.
  The test fails if this code crashes.
}
```

```
> for i in [1..100]: myTest(random());
```


# Testing: Exhaustive Property test

```
myTest (argument) {
  Run some code here.
  The test passes if this code does not crash.
  The test fails if this code crashes.
}
```

```
> for i in entireType: myTest(i);
```


# Testing: Specialised Exhaustive Property test

```
myTest (argument) {
  Run some code here.
  The test passes if this code does not crash.
  The test fails if this code crashes.
}
```

```
> for i in subSet: myTest(i);
```


# Testing: Specialised Random Property test

```
myTest (argument) {
  Run some code here.
  The test passes if this code does not crash.
  The test fails if this code crashes.
}
```

```
> for i in subSet: myTest(subSet.random());
```


# Property testing with QuickCheck

``` Haskell
class Arbitrary a where
  arbitrary :: Gen a

property :: (Show a, Arbitrary a) => (a -> Bool) -> Test
```

## Randomised Property Testing with QuickCheck

``` Haskell
λ> quickCheck myProperty
+++ OK, passed 100 tests.  -- Cool!
```

# What if not all values are valid

* Only generate valid values?
  No, then you are missing crucial values for safety.
* Generate unchecked values?
  No, then you cannot test functions that only handle valid values.
* Have seperate generators for each?
  No, then you lose the composability of testing:

``` Haskell
associative :: Arbitrary a => (a -> a -> a) -> Test
identity :: Arbitrary a => a -> (a -> a -> a) -> Test
monoid :: Arbitrary a => a -> (a -> a -> a) -> Test
monoid id op = associative op .&. identity id op
```

## Semantics: ???


# Testing: GenUnchecked  

``` Haskell
instance GenUnchecked Prime where
  genUnchecked = Prime <$> arbitrary
```

## Semantics: Any possible value.

``` Haskell
instance GenUnchecked MyType
  -- ^ GHC will figure it out.
```


# Testing: GenValid

``` Haskell
instance GenValid Prime where
  genValid = genUnchecked `suchThat` isValid 
    -- ^ Default implementation

-- Or something faster...

instance GenValid Prime where
    genValid = Prime <$>
       (oneof
         [ pure 2
         , ((\y -> 2 * abs y + 1) <$> arbitrary) `suchThat` isPrime)
         ])
```

# Validity Testing Properties

``` Haskell
failsOnInvalid
  :: (Show a, Show b, GenInvalid a)
  => (a -> Maybe b) -> Property

validIfSucceeds
  :: (Show a, Show b, GenUnchecked a, Validity b)
  => (a -> Maybe b) -> Property

succeedsOnValid
  :: (Show a, Show b, GenValid a)
  => (a -> Maybe b) -> Property
```

# Validity-based Testing Properties

``` Haskell
equivalentOnValid
  :: (Show a, Eq a, GenValid a, Show b, Eq b)
  => (a -> b) -> (a -> b) -> Property

associativeOnValids
  :: (Show a, Eq a, GenValid a)
  => (a -> a -> a) -> Property

inverseFunctionsIfSucceed
  :: (Show a, Eq a, GenUnchecked a, CanFail f, CanFail g)
  => (a -> f b) -> (b -> g a) -> Property
```

# Validity-based Testing Spec

``` Haskell
{-# LANGUAGE TypeApplications #-}

ordSpec :: forall a.
     (Show a, Ord a, Typeable a, GenUnchecked a)
  => Spec

ordSpecOnValid :: ...
```

# Validity-based Testing Spec

```
λ> ordSpec @Int

  Ord Int
    (<=) :: Int -> Int -> Bool
      is reflexive
      is antisymmetric
      is transitive
      is equivalent to (\a b -> compare a b /= GT)
    (>=) :: Int -> Int -> Bool
      is reflexive
      is antisymmetric
      is transitive
      is equivalent to (\a b -> compare a b /= LT)
    (<) :: Int -> Int -> Bool
      is antireflexive
      is transitive
      is equivalent to (\a b -> compare a b == LT)
    (>) :: Int -> Int -> Bool
      is antireflexive
      is transitive
      is equivalent to (\a b -> compare a b == GT)
```

# Validity-based Testing Spec

``` Haskell
{-# LANGUAGE TypeApplications #-}

functorSpec :: forall f.
    ( Eq (f Int), Show (f Int)
    , Functor f, Typeable f, GenUnchecked (f Int)
    ) 
    => Spec

functorSpecOnValid :: ...
```

# Validity-based Testing Spec

```
functorSpec @[]

Functor []
    fmap :: (a -> b) -> [] a -> [] b
      satisfies the first Fuctor law:
        'fmap id == id'
      satisfieds the second Functor law:
        'fmap (f . g) == fmap f . fmap g'
    (<$) :: a -> [] b -> [] a
      is equivalent to its default implementation
```

# Validity-based Aeson Spec

``` Haskell
jsonSpec :: forall a.
  ( Show a, Eq a, Typeable a, 
    GenUnchecked a, FromJSON a, ToJSON a)
  => Spec

jsonSpecOnValid :: ...
```

# Valdity-based Aeson Spec

```
λ> jsonSpec @Int

  JSON Int (unchecked)
    encode :: Int -> Data.ByteString.Lazy.ByteString
      never fails to encode
    decode :: Int -> Data.ByteString.Lazy.ByteString
      ensures that encode and decode are inverses
```

# Validity-based Lens Spec

```
λ> lensSpecOnValid ((_2) :: Lens' (Double, Double) Double)

  lensSpecOnValid
    satisfies the first lens law
    satisfies the second lens law
    satisfies the third lens law
    gets valid values
    produces valid values
```

# Cost

```
data MyType = MyType Int Text
  deriving (Show, Eq, Generic)
instance GenUnchecked MyType
instance Validity MyType
instance GenValid MyType
```

# Other libraries

```
genvalidity-hspec
genvalidity-hspec-aeson
genvalidity-hspec-binary
genvalidity-hspec-cereal
genvalidity-hspec-hashable
genvalidity-hspec-optics
```
