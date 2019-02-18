# Validity and validity-based testing

[![Build Status](https://travis-ci.org/NorfairKing/validity.svg?branch=master)](https://travis-ci.org/NorfairKing/validity)

## Why?

> Make write correct software cheaper.

- Cheap properties
- Free generators
- Free shrinking

### Cheap properties

- Property Combinators:

``` haskell
specify "inverse functions" $ inverseFunctions (+1) (-1)
specify "equivalent functions" $ equivalent ((+ 1) . (- 1)) id
specify "transitivity" $ transitivity ((>) :: Int -> Int -> Bool)
specify "symmetry" $ symmetry ((==) :: Int -> Int -> Bool)
```

- Test suite combinators:

``` haskell
ordSpec @Int
```

```
  Ord Int
    (<=) :: Int -> Int -> Bool
      is reflexive for "even Int"'s
        +++ OK, passed 100 tests.
      is antisymmetric for "even Int"'s
        +++ OK, passed 100 tests.
      is transitive for "even Int"'s
        +++ OK, passed 100 tests.
      is equivalent to (\a b -> compare a b /= GT) for "even Int"'s
        +++ OK, passed 100 tests.
    (>=) :: Int -> Int -> Bool
      is reflexive for "even Int"'s
        +++ OK, passed 100 tests.
      is antisymmetric for "even Int"'s
        +++ OK, passed 100 tests.
      is transitive for "even Int"'s
        +++ OK, passed 100 tests.
      is equivalent to (\a b -> compare a b /= LT) for "even Int"'s
        +++ OK, passed 100 tests.
    (<) :: Int -> Int -> Bool
      is antireflexive for "even Int"'s
        +++ OK, passed 100 tests.
      is transitive for "even Int"'s
        +++ OK, passed 100 tests.
      is equivalent to (\a b -> compare a b == LT) for "even Int"'s
        +++ OK, passed 100 tests.
    (>) :: Int -> Int -> Bool
      is antireflexive for "even Int"'s
        +++ OK, passed 100 tests.
      is transitive for "even Int"'s
        +++ OK, passed 100 tests.
      is equivalent to (\a b -> compare a b == GT) for "even Int"'s
        +++ OK, passed 100 tests.
```

### Free generators

``` haskell
data MyType = MyType
  { myRational :: Rational
  , myBool :: Bool
  } deriving (Show, Eq, Generic)

instance Validity MyType -- Implementation is derived via Generic
instance GenUnchecked MyType -- Implementation is derived via Generic
instance GenValid MyType -- Default implementation via GenUnchecked and Validity
```

``` haskell
genValid :: Gen MyType -- Free access to a generator
```

### Free shrinking

``` haskell
shrinkValid :: MyType -> [MyType] -- Free access to a _valid_ shrinking function
```

## Further Documentation

- [Usage example](docs/USAGE_EXAMPLE.md)
- [Fully worked example](docs/FULLY_WORKED_EXAMPLE.md)
- [High-level package overview](docs/PACKAGE_OVERVIEW.md)
