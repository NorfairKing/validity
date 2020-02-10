# Changelog

## [Unreleased]

## [0.4.1.1] - 2020-02-10

### Changed

* Improved the cabal file

## [0.4.1.0] - 2019-02-28

### Added

* `Validity` instance for `ShortByteString`s

## [0.4.0.0] - 2018-08-25

### Added

* `Validity` instance for lazy `ByteString`s

### Changed

* `Validity` for `ByteString` is now more precise: It will actually look into the constructor, at the length and the offset.

## Older versions

No history before version 0.4.0.0
