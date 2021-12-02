# Changelog

## [1.0.0.1] - 2021-11-20

### Changed

* Added compatibility with `lts-18.16`

## [1.0.0.0] - 2021-11-20

### Changed

* The default implementation of `GenValid` now uses `genValidStructurally` and `shrinkValidStructurally`.

### Removed

* `GenUnchecked`: It is no longer necessary after changing the default implementation of `GenValid`.
* `GenInvalid`: It was a misfeature.
* `GenRelativeValidity`: It was a misfeature. 

## [0.11.0.1] - 2021-06-20

### Changed

Updated the base lower bound to be more accurate.

## [0.11.0.1] - 2020-04-28

### Changed

* Got rid of some unnecessary extra special values in the generator for double

## [0.11.0.0] - 2020-04-12

### Changed

* Changed the genUnchecked and shrinkUnchecked definitions for Word8, Word16, Word32 and Int8, Int16, Int32 according to their new validity instance in validity 0.10.0.0

## [0.10.0.2] - 2020-03-18

### Changed

* Better lower bound for genvalidity

## [0.10.0.1] - 2020-02-20

### Changed

* Fixed a bug where shrinking ratios of bounded types would crash if the minimum bound was in the numerator.

## [0.10.0.0] - 2020-02-10

### Added

* `Data.GenValidity.Utils.genIntX`
* `Data.GenValidity.Utils.genWordX`
* `Data.GenValidity.Utils.genFloat`
* `Data.GenValidity.Utils.genDouble`
* `Data.GenValidity.Utils.genFloatX`
* `Data.GenValidity.Utils.genInteger`


### Changed

* Improved the cabal file
* Sped up the 'genValid' generators for the following types
  * ()
  * Bool
  * Ordering
  * Char
  * Int, Int8, Int16, Int32, Int64
  * Word, Word8, Word16, Word32, Word64
  * Float, Double
* Improved the generators of
  * Int, Int8, Int16, Int32, Int64
    Now also generates extreme values, but mostly uniform values.
  * Word, Word8, Word16, Word32, Word64
    Now also generates extreme values, but mostly uniform values.
  * Float, Double
    Now also generates values around the bounds, but mostly uniform values.
  * Natural, Integer
    Now also generates numbers larger than can be contained in a single Word/Int.
  * Ratio
    Fixed a bug: no longer generates invalid ratios for fixed-sized numerators
* Removed a lot of shrinking tests

## [0.9.1.0] - 2019-12-04

### Added

* `genSplit6`, `genSplit7`, `genSplit8`
* `genNonEmptyOf`

### Changed

* Changed `arbPartition` to generate nicer partitions.
  This influences `genListOf` and `genTreeOf` and the
  instances for all collections as well.

## [0.9.0.1] - 2019-09-27

### Changed

* Tests for `genUtf16SurrogateCodePoint`

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

