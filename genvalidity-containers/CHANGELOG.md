# Changelog

## [1.0.0.0] - 2021-11-20

### Changed

* Compatibility with `genvalidity >= 1.0.0.0`

### Removed

* `genStructurallyValidMapOf`
* `genStructurallyValidMapOfInvalidValues`
* `genStructurallyInvalidMap`
* `genStructurallyValidSetOf`
* `genStructurallyValidSetOfInvalidValues`
* `genStructurallyInvalidSet`

## [0.9.0.0] - 2020-06-14

### Added

* `genMapOf`
* `genSeqOf`
* `genSetOf`

### Changed

* Improved the generation of Set, Seq and Map to generate more appropriately (bigger) sized collections

## [0.8.0.2] - 2020-02-10

### Changed

* Improved the cabal file
* Removed the show constraint for keys on 'GenUnchecked (Map k v)'
* Removed the shrinking tests for trees

## [0.8.0.1] - 2019-12-04

### Changed

* Changed the way trees are generated.
  They will no longer be as top-heavy or under-sized.

## [0.8.0.0] - 2019-09-23

### Changed

* No longer require a 'Show' instance of the map's key for `GenUnchecked`

## [0.7.0.0] - 2019-09-23

### Changed

* Compatibility with validity-containers >=0.5
* Test suite compatibility with genvalidity-property >=0.5

## [0.6.0.0] - 2019-03-06

### Changed

* Fixed type signatures to be compatible with genvalidity >=0.8

## [0.5.1.1] - 2018-11-07

### Changed

* Test suite compatibility with validity >=0.9

## [0.5.1.0] - 2018-10-06

### Changed

* Sped up `shrinkValid` for `Tree`
