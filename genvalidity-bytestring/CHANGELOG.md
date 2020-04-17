# Changelog

## [Unreleased]

## [0.6.0.0] - 2020-04-17

### Added

* genStrictByteStringBy
* genLazyByteStringBy
* genLazyByteStringByStrictByteString
* Benchmarks for the generators

### Changed

* Strict Bytestrings are now generated up to 4x faster.
* Lazy Bytestrings are now generated to usually have multiple chunks.

## [0.5.0.1] - 2020-02-10

* Improved the cabal file

## [0.5.0.0] - 2019-03-06

### Added

* genTrulyUncheckedLazyByteString
* shrinkTrulyUncheckedLazyByteString

### Changed

* Removed the 'GenUnchecked' and 'GenInvalid' instances for both strict and lazy `ByteString`s
* Poisoned those two instances as well.

## [0.4.1.0] - 2019-03-06

### Added

* 'GenUnchecked' and 'GenValid' for 'ShortByteString'

## [0.4.0.0] - 2019-03-06

### Added

* genTrulyUncheckedStrictByteString
* shrinkTrulyUncheckedStrictByteString

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

