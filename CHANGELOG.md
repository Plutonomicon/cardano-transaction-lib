# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and we follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Single `Plutus.Conversion` module exposing all `(Type <-> Plutus Type)` conversion functions.
- `balanceAndSignTxs` balances and signs multiple transactions while taking care to use transaction inputs only once.

### Removed

- `FromPlutusType` / `ToPlutusType` type classes.

### Changed

- `balanceAndSignTx` now locks transaction inputs within the current `Contract` context. If the resulting transaction is never used, then the inputs must be freed with `unlockTransactionInputs`.

## [1.0.0] - 2022-06-07

CTL's initial release!
