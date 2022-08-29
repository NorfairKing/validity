# Changelog

## [1.1.0.0] - 2022-08-30

### Added

* `shrinkValidBench`
* `shrinkBench`
* `shrinkBenchN`
* `shrinkBenchVector`

### Changed

* Renamed `genBenchSizes` to `genBench` and removed the old `genBench`.
* Changed the generator benchmarks to generate 100 values instead of 1.
  This corresponds to how long the generating part of a property test might take
  and corrects for some of the difference in generating different values.

## [1.0.0.0] - 2021-11-20

### Changed

* Compatibility with `genvalidity >= 1.0.0.0`

### Removed

* `genValidityBench`
* `genUncheckedBench`

## [0.2.0.0] - 2020-05-05

### Added

* genBenchSized

### Changed

* Benchmarking now happens with sizes 15 and 30 instead of 8,16,32,64.
  The reasoning is that you only need to see if the time stays constant or not, and what the time is for a generator with the default size 30
* Fixed a memory leak by using pure and deterministic generation instead of using IO-based pseudorandom generation.

## [0.1.0.0] - 2020-04-17

### Added

* genBenchSizes

### Changed

* genValidBench, genUncheckedBench and genValidityBench now benchmark with multiple sizes

