# Changelog

## [1.0.1.0] - 2025-12-25

### Changed

* Generalised spec combinators to have type `TestDef outers ()` instead of `Spec`.

## [1.0.0.0] - 2021-11-20

* Compatibility with `genvalidity >= 1.0.0.0`
* Renamed every combinator that ends in `OnValid` (or similar) to not have that suffix anymore.

### Removed

* Every combinator that relates to unchecked or invalid values.

