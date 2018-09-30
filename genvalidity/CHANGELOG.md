# Changelog

## [Unreleased]

### Changed

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

