# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and we follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

**Table of Contents**

- [[Unreleased]](#unreleased)
  - [Added](#added)
  - [Removed](#removed)
  - [Fixed](#fixed)
- [[1.0.1] - 2022-06-17](#101---2022-06-17)
  - [Fixed](#fixed-1)
- [[1.0.0] - 2022-06-10](#100---2022-06-10)

## [Unreleased]

### Added

- `mkKeyWalletFromFile` helper to use `cardano-cli`-style `skey`s.
- Single `Plutus.Conversion` module exposing all `(Type <-> Plutus Type)` conversion functions.
- Support for using a `PrivateKey` as a `Wallet`.
- `getProtocolParameters` call to retrieve current protocol parameters from Ogmios (#541).

### Removed

- `FromPlutusType` / `ToPlutusType` type classes.
- `Contract.Wallet.mkGeroWallet` and `Contract.Wallet.mkNamiWallet` - `Aff` versions should be used instead.
- Protocol param update setters for the decentralisation constant (`set_d`) and the extra entropy (`set_extra_entropy`).

### Changed

- Updated `ogmios-datum-cache` - bug fixes (#542, #526, #589).
- Improved error response handling for Ogmios (#584).
- Made protocol parameters part of `QueryConfig`.
- Refactored `Plutus.Conversion.Address` code (utilized CSL functionality).
- Changed the underlying type of `Slot`, `TransactionIndex` and `CertificateIndex` to `BigNum`.
- Moved transaction finalization logic to `balanceTx`.
- Upgraded to CSL v11.0.0-beta.1.
- Upgraded to Ogmios v5.5.0

### Fixed

- Handling of invalid UTF8 byte sequences in the Aeson instance for `TokenName`.
- `Types.ScriptLookups.require` function naming caused problems with WebPack (#593)
- Bad logging in `queryDispatch` that didn't propagate error messages (#615).
- `Types.ScriptLookups.require` function naming caused problems with WebPack (#593).
- UTxO Min-Ada-Value calculation.

## [1.0.1] - 2022-06-17

### Fixed

- `mustBeSignedBy` now sets the `Ed25519KeyHash` corresponding to the provided `PaymentPubKeyHash` directly. Previously, this constraint would fail as there was no way to provide a matching `PaymentPubKey` as a lookup. Note that this diverges from Plutus as the `paymentPubKey` lookup is always required in that implementation.

## [1.0.0] - 2022-06-10

CTL's initial release!
