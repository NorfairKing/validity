# Changelog

## [Unreleased]

## [0.3.0.0] - 2018-08-25

### Added

* `GenUnchecked`, `GenValid` and `GenInvalid`, instances for lazy `ByteString`.

### Changed

* `genUnchecked :: Gen ByteString` now generates much more unchecked values according to the new `validity-bytestring`.
  **If you experience unexpected segfaults, the problem could be that you are using unchecked `ByteString`s where you should be using valid `ByteString`s.**

## Older versions

No history before version 0.3.0.0

