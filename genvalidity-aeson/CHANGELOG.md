# Changelog

## [1.1.0.0] - 2022-08-30

### Changed

* Shrinking a `Value` now also shrinks to `Null` in every case.
* Sped up generating `Key` by going via `Text` instead of going via `String`

## [1.0.0.1] - 2022-04-26

### Added

* Support for `aeson >= 2.0.0.0`

## [1.0.0.0] - 2021-11-20

### Changed

* Compatibility with `genvalidity >= 1.0.0.0`
* Improved shrinking for 'Value'.

## [0.3.0.1] - 2020-02-10

### Added

* Added benchmarks.
* Added a test to make sure that it generates renderable values.

## [0.3.0.0] - 2019-03-06

### Changed

* Removed the 'GenUnchecked' and 'GenInvalid' instance for 'Value'.

## Older versions

No history before version 0.3.0.0

