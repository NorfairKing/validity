# Changelog

## [1.0.0.0] - 2021-11-20

* Compatibility with `genvalidity >= 1.0.0.0`
* Renamed every combinator that ends in `OnValid` (or similar) to not have that suffix anymore.

### Removed

* Every combinator that relates to unchecked or invalid values.

## [0.3.0.2] - 2020-02-10

### Changed

* Removed the doctests
* Improved the cabal file

## [0.3.0.1] - 2018-10-07

### Changed

* Fixed the order of the `shouldBe` in the roundtrip spec combinator
* Compatibility with validity >=0.9, genvalidity >=0.7 and genvalidity-property >=0.3
