---
title: Validity and Validity-based testing
author: Tom Sydney Kerckhove
---

# Cheaper testing: My wish for sensible functions

``` haskell
data Tag = Tag
  { tagName :: Text -- Invariant: No whitespace
  , tagValue :: Text -- Invariant: No newlines
  } deriving (Show, Eq, Generic)
                        
                            
                        
 


doSomethingUseful :: TagName -> TagName

spec :: Spec
spec = 
    <SOMETHING CHEAP>




```

# Cheaper testing: Validity for sensible functions

``` haskell
data Tag = Tag
  { tagName :: Text -- Invariant: No whitespace
  , tagValue :: Text -- Invariant: No newlines
  } deriving (Show, Eq, Generic)

instance Validity Tag where
  validate = [...] -- Explain the invariants
instance GenUnchecked Tag
instance GenValid Tag

doSomethingUseful :: TagName -> TagName

spec :: Spec
spec = 
  describe "Tag" $
    describe "doSomethingUseful" $
      it "produces valid values" $
        producesValidsOnValids doSomethingUseful
```


# Cheaper testing: My wish for sensible instances

``` Haskell
data MyType = MyType
  { myBool :: Bool
  , myDouble :: Double
  } deriving (Show, Eq, Generic)

instance FromJSON MyType where
  parseJSON = [...]
instance ToJSON MyType where
  toJSON = [...]

                        
                            
                        

spec :: Spec
spec =
   <SOMETHING EASY>                    
                           
```

# Cheaper testing: Validity for sensible instances

``` Haskell
data MyType = MyType
  { myBool :: Bool
  , myDouble :: Double
  } deriving (Show, Eq, Generic)

instance FromJSON MyType where
  parseJSON = [...]
instance ToJSON MyType where
  toJSON = [...]

instance Validity MyType
instance GenUnchecked MyType
instance GenValid MyType

spec :: Spec
spec =
  describe "MyType" $
    jsonSpecOnValid @MyType

```

# Cheaper testing: My wish for sensible monads

``` Haskell
data MStop a
    = Stop
    | Continue a
    deriving (Show, Eq, Generic)

instance Functor MStop where
  [...]
instance Applicative MStop where
  [...]
instance Monad MStop where
  [...]

                                         
                                                 
                                         

spec :: Spec
spec = do




```

# Cheaper testing: Validity for sensible monads

``` Haskell
data MStop a
    = Stop
    | Continue a
    deriving (Show, Eq, Generic)

instance Functor MStop where
  [...]
instance Applicative MStop where
  [...]
instance Monad MStop where
  [...]

instance Validity a => Validity (MStop a)
instance GenUnchecked a => GenUnchecked (MStop a)
instance GenValid a => GenValid (MStop a)

spec :: Spec
spec = do
  functorSpec @MkStop
  applicativeSpec @MkStop
  monadSpec @MkStop
```

# Cheaper testing: My wish for sensible lenses

``` Haskell

data MyType = MyType
  { myBool :: Bool
  , myDouble :: Double
  } deriving (Show, Eq, Generic)

myDoubleL :: Lens' MyType Double
myDoubleL = lens myDouble $ \mt d -> mt {myDouble = d}

                        
                            
                        

spec :: Spec
spec =
    <SOMETHING CHEAP THAT TESTS EVERYTHING ABOUT LENSES>                       
                             
```

# Cheaper testing: Validity for sensible lenses

``` Haskell

data MyType = MyType
  { myBool :: Bool
  , myDouble :: Double
  } deriving (Show, Eq, Generic)

myDoubleL :: Lens' MyType Double
myDoubleL = lens myDouble $ \mt d -> mt {myDouble = d}

instance Validity MyType
instance GenUnchecked MyType
instance GenValid MyType

spec :: Spec
spec =
  describe "myDoubleL" $
    lensSpecOnValid myDoubleL
```
