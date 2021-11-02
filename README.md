# Validity and validity-based testing

## Why?

> to make writing correct software cheaper.

- Free generators
- Free shrinking
- Cheap properties

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

### Free generators and shrinking

``` haskell
data MyType = MyType
  { myRational :: Rational
  , myBool :: Bool
  } deriving (Show, Eq, Generic)

instance Validity MyType -- Implementation is derived via Generic
instance GenValid MyType -- Default implementation via Generic
```

``` haskell
genValid :: Gen MyType -- Free generator
shrinkValid :: MyType -> [MyType] -- Free _valid_ shrinking function
```

## Cachix cache

There is a [cachix](https://cachix.org) cache for this project.

To use it, use `cachix use validity` or add the appropriate details to your nixos configuration.
