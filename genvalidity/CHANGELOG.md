# Changelog

## [0.9.0.0] - 2019-09-23

### Added

* `genUtf16SurrogateCodePoint`

### Changed

* Changed `GenValid Char` to generate UTF16 surrogate codepoints 10% of the time
* Changed `GenValid Char` to ignore sizes.

## [0.8.0.0] - 2019-03-06

### Added

* 'shrinkTuple'

### Changed

* Removed the 'GenUnchecked' constraint for 'GenValid' and 'GenInvalid'.

## [0.7.0.2] - 2019-02-28

### Added

* 'shrinkT4'

### Changed

* Clearer docs

## [0.7.0.1] - 2019-02-21

### Changed

* Sped up the shrinking test suite.

## [0.7.0.0] - 2018-11-07

### Changed

* `genUnchecked` of `Double` and `Float` now generates `NaN`, `+Infinity`, `-Infinity` and `-0` according to the new version of `validity`.

## [0.6.1.0] - 2018-10-06

### Changed

* Changed 'genValid`, `genUnchecked` and `genInvalid` for NonEmpty to better take the size into account.
* Sped up `shrinkUnchecked` for `Maybe`
* Sped up `shrinkValid` for `Maybe`
* Sped up `shrinkUnchecked` for `Either`
* Sped up `shrinkValid` for `Either`
* Sped up `shrinkUnchecked` for `(,)`
* Sped up `shrinkUnchecked` for `(,,)`
* Sped up `shrinkUnchecked` for `(,,,)`
* Sped up `shrinkValid` for lists
* Sped up `shrinkValid` for `NonEmpty` lists

## [0.6.0.0] - 2018-08-25

### Added

* `genValidStructurally` and `genValidStructurallyWithoutExtraChecking`
* `shrinkValidStructurally` and `shrinkValidStructurallyWithoutExtraFiltering` with `structurallyValidRecursivelyShrink` and `structurallyValidSubterms`

### Changed

* `-0` is now a valid value for `Double` and `Float`.
* `genUnchecked :: Gen Double` now also generates invalid values.
* `arbPartition` now shuffles the partitions, which means that `genListOf` produces lists of elements with shuffled sizes.
  This also fixes the same problem with `instance GenUnchecked a => GenUnchecked [a]`.

## Older versions

No history before version 0.6.0.0

