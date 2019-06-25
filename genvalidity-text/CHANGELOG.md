# Changelog

## [Unreleased]

## [0.7.0.0] - 2019-06-26

* Updated the 'GenValid Text' instance to use 'genValid :: Gen Char' instead of 'arbitrary :: Gen Char'.

### Changed

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

