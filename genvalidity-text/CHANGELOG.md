# Changelog

## [Unreleased]

## [0.7.0.2] - 2020-02-10

## Changed

* Improved the cabal file

## [0.7.0.1] - 2019-12-05

## Changed

* Optimised the 'GenValid Text' instance to be 5x faster.

## Added

* `genText` with the new approach.
* `genTextBy` with the new approach where you can supply your own `Gen Char`.


## [0.7.0.0] - 2019-06-26

## Changed

* Updated the 'GenValid Text' instance to use 'genValid :: Gen Char' instead of 'arbitrary :: Gen Char'.

## [0.6.0.0] - 2019-03-06

### Changed

* Removed the 'GenUnchecked' and 'GenInvalid' instance for Text.
* Poisoned those two instances as well.
* Removed an 'upTo' from the implementation of 'GenValid Text'.
  It should generate better sized Text now.

## [0.5.1.0] - 2018-08-25

### Added

* A `GenUnchecked`, `GenValid` and `GenInvalid` instance for lazy text

## Older versions

No history before version 0.5.1.0

