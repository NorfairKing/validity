# A usage example

## Usage example with derived instances

Assume you have some data type:

``` Haskell
data MyType = MyType
  { myBool :: Bool
  , myRational :: Rational
```

### Step 1: derive `Generic`:

``` Haskell
  } deriving (Show, Eq, Generic)
```

### Step 2: instantiate `Validity`:

``` Haskell
instance Validity MyType
```

The implementation is generated because `MyType` instantiates `Generic`.

### Step 3: instantiate `GenUnchecked` and `GenValid`:

``` Haskell
instance GenUnchecked MyType
instance GenValid MyType
```

Again, the implementation is generated because `MyType` instantiates `Generic`.

(Use `genValidStructurally` and `shrinkValidStructurally` if `GenUnchecked` is disabled.)

### Step 4: Write simple instance tests using test suite combinators:

``` Haskell
spec :: Spec
spec = do
  eqSpec @MyType
  genValidSpec @MyType
```

These two lines will generate the test suite that has output as follows:

```
  Eq UUID User
    (==) :: MyType -> MyType -> Bool
      is reflexive for "unchecked MyType"s
      is symmetric for "unchecked MyType"s
      is transitive for "unchecked MyType"s
      is equivalent to (\a b -> not $ a /= b) for "unchecked MyType"s
    (/=) :: MyType -> MyType -> Bool
      is antireflexive for "unchecked MyType"s
      is equivalent to (\a b -> not $ a == b) for "unchecked MyType"s
  GenValid MyType
    genValid   :: Gen MyType
      only generates valid 'MyType's
```

### Conclusion

The following few lines are all that you need to get started with validity-based testing:

``` Haskell
data MyType = MyType
  { myBool :: Bool
  , myRational :: Rational
  } deriving (Show, Eq, Generic)

instance Validity MyType
instance GenUnchecked MyType
instance GenValid MyType

spec :: Spec
spec = do
  eqSpec @MyType
  genValidSpec @MyType
```


## Usage example with custom instances

Assume the following `Prime` `newtype` and an `isPrime` function:

``` Haskell
newtype Prime = Prime Int
isPrime :: Int -> Bool
``` 

### Validity

Define explicit validity for `Prime`:

``` Haskell
instance Validity Prime where
    validate (Prime i) = check (isPrime i) "the contained integer is a prime"
```

### GenUnchecked, GenValid and GenInvalid

Define generators for valid and invalid primes:

``` Haskell
instance GenUnchecked Prime where
    genUnchecked = Prime <$> arbitrary

instance GenValid Prime where
    genValid = Prime <$>
       (oneof
         [ pure 2
         , ((\y -> 2 * abs y + 1) <$> arbitrary) `suchThat` isPrime)
         ])

instance GenInvalid Prime
```

### Genvalidity Hspec

Given a smart constructor for `Prime`s:

``` Haskell
prime :: Int -> Maybe Prime
```

We can now very easily write tests for functions that involve `Prime`s.

``` Haskell
describe "prime" $ do
  it "fails on invalid input" $ do
    failsOnInvalid prime

  it "succeeds on valid input input" $ do
    succeedsOnValid prime

  it "produces valid output when it succeeds" $ do
    validIfSucceeds prime
```

### Overriding automatic instances

For product types, the automatically derived instances will assume that the product is valid if all the fields are valid.
There may be further requirements.

```Haskell
data MyType = MyType
  { myBool :: Bool
  , myRational :: Rational
  } deriving (Show, Eq, Generic)
  
instance Validity MyType
```

The derived instance is equivalent to the following:

```Haskell
instance Validity MyType where
  validate mt = genericValidate mt
```

This can be extended.
For example, if `myBool` should indicate whether `myRational` is nonnegative (>= 0), the following instance might be appropriate.

```Haskell
instance Validity MyType where
  validate mt = mconcat
    -- Test invariant above
    [ declare "myBool indicates whether myRational is nonnegative" $
        myBool mt `xor` (myRational mt < 0)
    -- Use of 'decorate' is optional
    , decorate "MyType fields themselves are valid" $ genericValidate mt
    ]
```
