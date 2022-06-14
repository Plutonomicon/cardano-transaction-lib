# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and we follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Single `Plutus.Conversion` module exposing all `(Type <-> Plutus Type)` conversion functions.
- Support for using a `PrivateKey` as a `Wallet`.
- Upgraded `ogmios-datum-cache` to `54ad2964af07ea0370bf95c0fed71f60a778ead5` for more stable datum from datum hash.

### Removed

- `FromPlutusType` / `ToPlutusType` type classes.


## [1.0.0] - 2022-06-10

CTL's initial release!
