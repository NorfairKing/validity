# Changelog

## [Unreleased]

## [0.5.0.0] - 2019-03-06

### Added

* genTrulyUncheckedLazyByteString
* shrinkTrulyUncheckedLazyByteString

### Changed

* Removed the 'GenUnchecked' instance for both strict and lazy `ByteString`s

## [0.4.0.0] - 2019-03-06

### Changed

* The 'genUnchecked' instance of 'Strict.ByteString' and 'Lazy.ByteString' now `error` instead of generating values that segfault.

## [0.3.0.2] - 2019-02-28

### Changed

* Clearer documentation

## [0.3.0.1] - 2018-10-07

### Changed

* Test suite compatibility with `validity >= 0.9`

## [0.3.0.0] - 2018-08-25

### Added

* `GenUnchecked`, `GenValid` and `GenInvalid`, instances for lazy `ByteString`.

### Changed

* `genUnchecked :: Gen ByteString` now generates much more unchecked values according to the new `validity-bytestring`.
  **If you experience unexpected segfaults, the problem could be that you are using unchecked `ByteString`s where you should be using valid `ByteString`s.**

## Older versions

No history before version 0.3.0.0

