# Changelog

## [Unreleased]

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

