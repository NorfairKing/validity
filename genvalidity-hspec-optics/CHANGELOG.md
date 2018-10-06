# Changelog

## [Unreleased]

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
