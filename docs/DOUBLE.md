---
title: The Double Situation
---

This document discusses the `Validity` instance for `Double` (and `Float`).


## History

Up to version `0.8.0.0`, the `Validity` instance for `Double` (and `Float`) was implemented as follows:

``` haskell
instance Validity Double where
    validate d =
        mconcat
            [ declare "The Double is not NaN." $ not (isNaN d)
            , declare "The Double is not infinite." $ not (isInfinite d)
            ]
```

From version `0.9.0.0`, the `Validity` instance for `Double` (and `Float`) will be implemented such that values are trivially valid. This means that as of version `0.9.0.0`, `NaN`, `+Infinity` and `-Infinity` are considered valid.

```
instance Validity Double where
    validate = trivialValidation
```

## What validity means

If a value of a given type is considered invalid according to its `Validity` instance, that means that barring programmer errors, such a value never occurs at runtime.
This means that a real use-case must never have to deal with values that are considered invalid.
(Exceptions to this rule are cases where `constructValid` is used immediately on a value that was just constructed.)

## Considerations

To define the validity of `Double`, we have to ask ourselves at least the following questions:

- Should `-0.0` be valid?
- Should `NaN` be valid?
- Should `+Infinity` and `-Infinity` be valid?

Up to version `0.8.0.0`, `-0.0` was considered, valid, but `NaN`, `-Infinity` and `+Infinity` were not.
In version `0.9.0.0`, the answer to all of these questions is "Yes".

### `-0.0`

`-0.0` can be constructed using `read "-0.0" :: Double`.
For reference, it has the following characteristics:

```
> isNegativeZero (read "-0.0" :: Double)
True
> isIEEE (read "-0.0" :: Double)
True
> isDenormalized (read "-0.0" :: Double)
False
> 0.0 == read "-0.0"
True
> decodeFloat (read "0.0" :: Double)
(0,0)
```

The reasoning for `-0.0` to be considered valid is that it is considered equal to `0.0`.

### `NaN`, `+Infinity` and `-Infinity`

* `NaN` can be constructed as `read "NaN" :: Double`
* `+Infinity` can be constructed as `read "Infinity" :: Double`
* `-Infinity` can be constructed as `read "-Infinity" :: Double`

For reference, they have the following characteristics:

```
> let nan = read "NaN" :: Double
> isNaN nan
True
> isDenormalised nan
False
> isInfinite nan
False
> nan == nan
False
> nan /= nan
True
> nan >= nan
False
> nan > nan
False
> compare nan nan
GT
> nan <= nan
False
> nan < nan
False

> let pinf = read "Infinity" :: Double
> isNaN pinf
False
> isDenormalised pinf
False
> isInfinite pinf
True
> pinf == pinf
True
> pinf /= pinf
False
> pinf >= pinf
True
> pinf > pinf
False
> pinf < pinf
False
> pinf <= pinf
True
> compare pinf pinf
EQ

let minf = read "-Infinity" :: Double
> isNaN minf
False
> isDenormalised minf
False
> isInfinite minf
True
> minf == minf
True
> minf /= minf
False
> minf >= minf
True
> minf > minf
False
> minf <= minf
True
> minf < minf
False
> compare minf minf
EQ
```

### `Double` is a spec

The `Double` type is not a type that represents numbers, per se.
It is not a type of real numbers or even rational numbers.
It is _just_ a spec. In particular, it is part of [IEEE 754](https://en.wikipedia.org/wiki/IEEE_754).
Nothing more, nothing less.

It is true that many programmers will use `Double` as if it was a number, but that doesn't mean that that is a good idea.
`NaN` and the infinities, are values described in this spec and we should be able to deal with them.

There are cases where programs legitimately have to handle the case where a `Double` is `NaN`.
For example, when serialising `NaN`, we expect `NaN` to be deserialisable from the result.

If `Double` is not considered a rational or real number, then there is no more reason to not consider the infinities valid values.

### The `Eq` and `Ord` instances

One argument against considering `NaN` and the infinities valid, is that the `Eq` and `Ord` instances are then broken for `Double` for valid values.
In particular, `==` from `Eq` is not reflexive, because `nan == nan` evaluates to False.
Similarly, `<=` and `>=` are not reflexive, because `nan >= nan` and `nan <= nan` evaluate to False.

However:
- These cases are broken as intended, according to the IEEE 754 Spec.
- Not testing code where `NaN` is used because `NaN` involves broken instances, even if it makes tests pass where they otherwise should not, does not prevent this broken code to be triggered in production.


### Our own data types' `Validity` instances

In cases where `Double` is used to represent a rational number, often it _is_ the case that `NaN` and the infinities are invalid values _within the custom type_.
In that case, the user should define the `Validity` instances of their type that contains `Double` to considered a value containing `NaN` invalid.
Helper functions are provided in `Data.Validity` to help with this use-case.

### Our own data types' `Eq` and `Ord` instances

Because `Eq` and `Ord` are broken for `Double`, the derived `Eq` and `Ord` instances for custom data types that contain `Double` values will also be broken.
This means that testing those will be more complicated if `NaN` is considered valid.
When making our own data type containing a `Double`, we must either ensure that the `Eq` instance is valid, or that there is no such instance.
To make sure that the `Eq` and `Ord` instances are valid, we can either outlaw `NaN` for the `Double` value or write a custom instance that follows the appropriate laws.

### A better generator

The `arbitrary` generator in QuickCheck's `Arbitrary` does not generate `NaN` values, which is why this problem has been dorment until now.
Whether `NaN` is valid or not, is independent of whether `genUnchecked` should generate `NaN` values.
`genUnchecked` must generate `NaN` values regardles of whether `NaN` is considered valid.
If `NaN` is valid, `genValid` must also generate `NaN`.
This way, false-positives are less likely to occur than they would be when using `arbitrary`.
