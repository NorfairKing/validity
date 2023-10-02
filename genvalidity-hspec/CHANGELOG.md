# Changelog

## [1.0.0.3] - 2022-10-04

### Changed

* Compatibility with `hspec-core >= 2.11`.

## [1.0.0.2] - 2022-09-20

Same as 1.0.0.1, but with the right commit.

## [1.0.0.1] - 2022-09-02

### Changed

* Compatibility with `hspec-core >= 2.10`

## [1.0.0.0] - 2021-11-20

### Changed

* Compatibility with `validity >= 0.12.0.0`
* Compatibility with `genvalidity >= 1.0.0.0`
* Renamed every combinator that ends in `OnValid` (or similar) to not have that suffix anymore.

### Removed

* Every combinator that relates to unchecked or invalid values.
* Everything related to `RelativeValidity`.


## [0.7.0.3] - 2020-02-10

### Changed

* Removed doctests
* Improved the cabal file
* Fixed the `monadSpec` to not generate the list length using `genUnchecked`

## [0.7.0.2] - 2019-09-23

* Removed nonsense shrinking from `genValidSpec` and `genInvalidSpec`.

## [0.7.0.1] - 2019-09-23

* Removed nonsense shrinking from `arbitraryGeneratesOnlyValid`, `genValidGeneratesValid` and `genInvalidGeneratesInvalid`.

## [0.7.0.0] - 2019-03-06

### Changed

* Fixed compatibility with genvalidity >=0.8

## [0.6.2.3] - 2019-02-28

### Changed

* Clearer docs

## [0.6.2.2] - 2019-01-09

### Changed

* Fixed a forward incompatibility with hspec 2.6.x.

## [0.3.0.1] - 2018-10-07

### Changed

* Compatibility with validity >=0.9, genvalidity >=0.7 and genvalidity-property >=0.3
