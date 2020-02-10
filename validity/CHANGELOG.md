# Changelog

## [Unreleased]

## [0.9.0.3] - 2020-02-10

### Changed

* Improved the 'validateRatioNormalised' to not crash on certain extreme values.

## [0.9.0.2] - 2019-09-27

### Added

* `isUtf16SurrogateCodePoint` and `validateCharNotUtf16SurrogateCodePoint`

### Changed

* The contents of the error message when using `validateNotNan` or `validateNotInfinite` is now more accurate.

### Added

* `validateRatioNotNan`
* `validateRatioNotInfinite`
* `validateRatioNormalised`

## [0.9.0.1] - 2018-12-05

### Changed

* The validity instance of `Ratio a` now disallows unnormalised values.
  So `0 %: 1` is valid, but `0 %: 2` is not.

## [0.9.0.0] - 2018-10-07

### Added

* `prettyValidate`, `validationIsValid`, `prettyValidation`
* `validateNotNaN`, `validateNotInfinite`

### Changed

* Renamed `prettyValidation` to `prettyValidate` before adding the new `prettyValidation`.
* `NaN`, `+Infinity` and `-Infinity` are now considered valid for both `Double` and `Float`.

## [0.8.0.0] - 2018-08-25

### Added
* `decorateList` in `Data.Validity`

### Changed
* `-0` is now a valid value for `Double` and `Float`.

## Older versions

No history before version 0.8.0.0
