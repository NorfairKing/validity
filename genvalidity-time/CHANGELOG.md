# Changelog

## [1.0.0.1] - 2022-08-30

### Changed

* Sped up shrinking of `TimeOfDay`, `LocalTime` and `TimeOfDay`
* Shrinking benchmarks

## [1.0.0.0] - 2021-11-20

### Changed

* Compatibility with `genvalidity >= 1.0.0.0`
* Changed the `GenValid Day` instance to only generate days around 2020, no more millions or billions of years ago.

## [0.3.0.0] - 2020-02-10

### Added

* Benchmarks for all the types except 'TimeLocale'

### Changed

* Sped up 'GenValid TimeZoneName' and made it more varied.
  It now generates:
  - Empty
  - Any three characters
  - a ±HHMM
  - Any string
* Sped up 'GenValid TimeOfDay'
* Sped up 'GenValid UTCTime'
* Sped up 'GenUnchecked NominalDiffTime' on time >= 1.9.1
* Sped up 'GenValid NominalDiffTime'

## [0.2.1.2] - 2020-02-10

### Changed

* Improved the cabal file
* Fixed the 'GenValid Day' day instance to conform to the new 'Validity Day' instance.
* Removed the shrinking tests
