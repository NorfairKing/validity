# Changelog

## [1.0.0.0] - 2021-11-20

* Compatibility with `genvalidity >= 1.0.0.0`
* Renamed every combinator that ends in `OnValid` (or similar) to not have that suffix anymore.

### Removed

* Every combinator that relates to unchecked or invalid values.

## [0.1.1.2] - 2020-02-10

### Changed

* Removed the doctests
* Improved the cabal file


## [0.1.1.1] - 2018-11-07

### Changed

* Test suite compatibility with `validity >=0.9`, `genvalidity >=0.7` and `genvalidity-property >=0.3`

## [0.1.1.0] - 2018-10-06

### Added

* `shrinkUncheckedDoesNotShrinkToItself`
* `shrinkUncheckedDoesNotShrinkToItselfWithLimit`
* `shrinkValidDoesNotShrinkToItself`
* `shrinkValidDoesNotShrinkToItselfWithLimit`
* `shrinkInvalidDoesNotShrinkToItself`
* `shrinkInvalidDoesNotShrinkToItselfWithLimit`

### Changed

* Added the concept that shrinkValid should not shrink to itself to `shrinkValidSpec` and `shrinkValiditySpec`.

## [0.1.0.0] - 2018-08-25

### Added

* `lensGettingProducesValidOnValid`
* `lensGettingProducesValid`
* `lensGettingProducesValidOnArbitrary`
* `lensGettingProducesValidOnGen `
* `lensSettingProducesValidOnValid`
* `lensSettingProducesValid`
* `lensSettingProducesValidOnArbitrary`
* `lensSettingProducesValidOnGen`

### Changed

* `lensSpec`, `lensSpecOnArbitrary`, `lensSpecOnGen` and `lensSpecOnValid` now also include `lensGettingProducesValid*` and `lensSettingProducesValid*`

## [0.0.0.0] - 2018-06-28

### Added

* `lensLaw1`
* `lensLaw2`
* `lensLaw3`
* `lensSpec`
* `lensSpecOnArbitrary`
* `lensSpecOnGen`
* `lensSpecOnValid`
