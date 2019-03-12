# Validity and validity-based testing

[![Build Status](https://travis-ci.org/NorfairKing/validity.svg?branch=master)](https://travis-ci.org/NorfairKing/validity)

| package  | hackage  |
|---|---|
| validity  | [![Hackage](https://img.shields.io/hackage/v/validity.svg)](https://hackage.haskell.org/package/validity) |
| validity-aeson |  [![Hackage](https://img.shields.io/hackage/v/validity-aeson.svg)](https://hackage.haskell.org/package/validity-aeson) |
| validity-bytestring |  [![Hackage](https://img.shields.io/hackage/v/validity-bytestring.svg)](https://hackage.haskell.org/package/validity-bytestring) |
| validity-containers |  [![Hackage](https://img.shields.io/hackage/v/validity-containers.svg)](https://hackage.haskell.org/package/validity-containers) |
| validity-path |  [![Hackage](https://img.shields.io/hackage/v/validity-path.svg)](https://hackage.haskell.org/package/validity-path) |
| validity-primitive |  [![Hackage](https://img.shields.io/hackage/v/validity-primitive.svg)](https://hackage.haskell.org/package/validity-primitive) |
| validity-scientific |  [![Hackage](https://img.shields.io/hackage/v/validity-scientific.svg)](https://hackage.haskell.org/package/validity-scientific) |
| validity-text |  [![Hackage](https://img.shields.io/hackage/v/validity-text.svg)](https://hackage.haskell.org/package/validity-text) |
| validity-time |  [![Hackage](https://img.shields.io/hackage/v/validity-time.svg)](https://hackage.haskell.org/package/validity-time) |
| validity-unordered-containers |  [![Hackage](https://img.shields.io/hackage/v/validity-unordered-containers.svg)](https://hackage.haskell.org/package/validity-unordered-containers) |
| validity-uuid |  [![Hackage](https://img.shields.io/hackage/v/validity-uuid.svg)](https://hackage.haskell.org/package/validity-uuid) |
| validity-vector |  [![Hackage](https://img.shields.io/hackage/v/validity-vector.svg)](https://hackage.haskell.org/package/validity-vector) |
| genvalidity |  [![Hackage](https://img.shields.io/hackage/v/genvalidity.svg)](https://hackage.haskell.org/package/genvalidity) |
| genvalidity-aeson | [![Hackage](https://img.shields.io/hackage/v/genvalidity-aeson.svg)](https://hackage.haskell.org/package/genvalidity-aeson) |
| genvalidity-bytestring |  [![Hackage](https://img.shields.io/hackage/v/genvalidity-bytestring.svg)](https://hackage.haskell.org/package/genvalidity-bytestring) |
| genvalidity-containers |  [![Hackage](https://img.shields.io/hackage/v/genvalidity-containers.svg)](https://hackage.haskell.org/package/genvalidity-containers) |
| genvalidity-path |  [![Hackage](https://img.shields.io/hackage/v/genvalidity-path.svg)](https://hackage.haskell.org/package/genvalidity-path) |
| genvalidity-property |  [![Hackage](https://img.shields.io/hackage/v/genvalidity-property.svg)](https://hackage.haskell.org/package/genvalidity-property) |
| genvalidity-scientific |  [![Hackage](https://img.shields.io/hackage/v/genvalidity-scientific.svg)](https://hackage.haskell.org/package/genvalidity-scientific) |
| genvalidity-text |  [![Hackage](https://img.shields.io/hackage/v/genvalidity-text.svg)](https://hackage.haskell.org/package/genvalidity-text) |
| genvalidity-time |  [![Hackage](https://img.shields.io/hackage/v/genvalidity-time.svg)](https://hackage.haskell.org/package/genvalidity-time) |
| genvalidity-unordered-containers |  [![Hackage](https://img.shields.io/hackage/v/genvalidity-unordered-containers.svg)](https://hackage.haskell.org/package/genvalidity-unordered-containers) |
| genvalidity-uuid |  [![Hackage](https://img.shields.io/hackage/v/genvalidity-uuid.svg)](https://hackage.haskell.org/package/genvalidity-uuid) |
| genvalidity-vector |  [![Hackage](https://img.shields.io/hackage/v/genvalidity-vector.svg)](https://hackage.haskell.org/package/genvalidity-vector) |
| genvalidity-hspec |  [![Hackage](https://img.shields.io/hackage/v/genvalidity-hspec.svg)](https://hackage.haskell.org/package/genvalidity-hspec) |
| genvalidity-hspec-aeson |  [![Hackage](https://img.shields.io/hackage/v/genvalidity-hspec-aeson.svg)](https://hackage.haskell.org/package/genvalidity-hspec-aeson) |
| genvalidity-hspec-binary |  [![Hackage](https://img.shields.io/hackage/v/genvalidity-hspec-binary.svg)](https://hackage.haskell.org/package/genvalidity-hspec-binary) |
| genvalidity-hspec-cereal |  [![Hackage](https://img.shields.io/hackage/v/genvalidity-hspec-cereal.svg)](https://hackage.haskell.org/package/genvalidity-hspec-cereal) |
| genvalidity-hspec-hashable |  [![Hackage](https://img.shields.io/hackage/v/genvalidity-hspec-hashable.svg)](https://hackage.haskell.org/package/genvalidity-hspec-hashable) |
| genvalidity-hspec-optics | [![Hackage](https://img.shields.io/hackage/v/genvalidity-hspec-optics.svg)](https://hackage.haskell.org/package/genvalidity-hspec-optics) |


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
