# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)

## [Unreleased]

### `genvalidity`
- Made the following instances:
  - `(GenUnchecked a, GenUnchecked b, GenUnchecked c, GenUnchecked d, GenUnchecked e) => GenUnchecked (a, b, c, d, e)`
  - `(GenValid a, GenValid b, GenValid c, GenValid d, GenValid e) => GenValid (a, b, c, d, e)`
  - `(GenInvalid a, GenInvalid b, GenInvalid c, GenInvalid d, GenInvalid e) => GenInvalid (a, b, c, d, e)`

### `genvalidty-containers`:
- Made the `GenUnchecked` and `GenInvalid` instances of `Set` and `Map` more precise: they can now generate structurally invalid values as well.
