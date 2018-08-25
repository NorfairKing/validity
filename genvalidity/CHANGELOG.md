# Changelog

## [Unreleased]

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

