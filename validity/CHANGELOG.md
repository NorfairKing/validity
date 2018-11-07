# Changelog

## [Unreleased]

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
