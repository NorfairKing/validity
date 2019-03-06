---
title: The ByteString Situation
---

This document discusses the `GenUnchecked` instance for `ByteString`.

## What to do if an error message points you to this document

If you are reading this, you probably saw this error message:

``` Haskell
Data.GenValidity.ByteString.genUnchecked :: Strict.ByteString
You probably do not want to use this.
You probably want to use 'genValid' instead.
See https://github.com/NorfairKing/validity/blob/master/docs/BYTESTRING.md
```

- If you see this message because you used `genUnchecked :: Gen ByteString`, use `genValid :: Gen ByteString` instead.

- If you see this message because you used `genValid :: Gen FooBar` where `FooBar` is a custom type of your own making, then ask yourself the following question:

  Is your situation as follows?

  - You derived `GenUnchecked` using `Generic` like this:
    ```
    instance GenUnchecked FooBar
    ```

  - You implemented `GenValid` using its default method:
    ```
    instance GenValid FooBar
    ```

  Then override `genValid` using `genValidStructurally`.

## History

Up to (but not including) version `0.4.0.0` of `genvalidity-bytestring`, the
`GenUnchecked` instance of `ByteString` and `Text` were principled and would generate
values that were significantly dangerous (to the point where they would cause segfaults).

From version `0.4.0.0` of `genvalidity-bytestring`, these instances call `error` instead.
The `GenInvalid` instance have been removed as well.

As of version `0.5.0.0` of `genvalidity-bytestring`, which was enabled by version `0.8.0.0` of `genvalidity`,
the `GenUnchecked` instance of `ByteString`s has been removed entirely.

## Considerations

- Unchecked `ByteString`s cannot be shown because that would cause a segfaults.
- Unchecked `ByteString`s cannot be compared for equality (or ordering) because that would cause segfaults.
- This means that there is no real reason to ever generate an unchecked `ByteString`.

- Many things can go wrong when the old version of `genUnchecked :: Gen ByteString` (or `shrinkUnchecked`) is used,
  and `genUnchecked :: Gen ByteString` can be used unknowingly in many situations.
  For example, consider this code:

  ``` Haskell
  data MyType = MyType ByteString Bool
    deriving (Show, Eq, Generic)
  instance Validity MyType
  instance GenUnchecked MyType
  instance GenValid MyType
  ```

  When `genValid :: Gen MyType` is called, that's equivalent to calling the following:

  ``` Haskell
  (MyType <$> genUnchecked <*> genUnchecked) `suchThat` isValid
  ```

  That means that this generator could segfault, and even if the generator doesn't, the test most likely will.

  In the case where your custom type contains a `ByteString`, you always want to override `GenValid`, even if it is just with `genValidStructurally` such that `genValid` becomes the following:

  ``` Haskell
  (MyType <$> genValid <*> genValid) `suchThat` isValid
  ```

  In the new situation with the `error` implementation, a user will not get any segfaults but instead a nice error message, pointing them to this document.
  In the most recent situation, the `GenUnchecked` instance has been removed entirely.
