# Changelog

## [Unreleased]

## [0.5.0.2] - 2020-02-10

### Changed

* Removed the doctests
* Improved the cabal file

## [0.5.0.1] - 2019-09-23

### Changed

* Removed a redundant import to have a clean warning-free build

## [0.5.0.0] - 2019-09-23

### Changed

* Started using `pretty-show` for the output of `validIfSucceeds`, `validIfSucceedsOnGens2` and `validIfSucceedsOnValids3`
* Started using `pretty-show` for the output of `shouldBeValid` and `shouldBeInvalid`.
* Gave `genGeneratesValid` and `genGeneratesInvalid` much nicer output.
* Removed nonsense shrinking from `genGeneratesValid` and `genGeneratesInvalid`.

## [0.4.0.0] - 2019-03-08

### Changed

* Compatibility with genvalidity >=0.8

## [0.3.0.0] - 2018-11-07

### Changed

* Compatibility with validity >=0.9 and genvalidity >= 0.7

## [0.2.1.1] - 2018-10-06

### Added
* `shrinkDoesNotShrinkToItself`
* `shrinkDoesNotShrinkToItselfWithLimit`
* `shrinkDoesNotShrinkToItselfOnValid`
* `shrinkDoesNotShrinkToItselfOnValidWithLimit`
* `shrinkDoesNotShrinkToItselfOnInvalid`
* `shrinkDoesNotShrinkToItselfOnInvalidWithLimit`
* `doesNotShrinkToItself`
* `doesNotShrinkToItselfWithLimit`

### Changed

* exported `shrinkingPreservesWithLimit` from `Test.Validity.Shrinking.Property`
