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

- `mkKeyWalletFromFile` helper to use `cardano-cli`-style `skey`s
- Single `Plutus.Conversion` module exposing all `(Type <-> Plutus Type)` conversion functions ([#464](https://github.com/Plutonomicon/cardano-transaction-lib/pull/464))
- Support for using a `PrivateKey` as a `Wallet`
- `getProtocolParameters` call to retrieve current protocol parameters from Ogmios ([#541](https://github.com/Plutonomicon/cardano-transaction-lib/issues/541))
- `Contract.Utxos.getWalletBalance` call to get all available assets as a single `Value` ([#590](https://github.com/Plutonomicon/cardano-transaction-lib/issues/590))
- `balanceAndSignTxs` balances and signs multiple transactions while taking care to use transaction inputs only once
- Ability to load stake keys from files when using `KeyWallet` ([#635](https://github.com/Plutonomicon/cardano-transaction-lib/issues/635))
- Implement utxosAt for `KeyWallet` ([#617](https://github.com/Plutonomicon/cardano-transaction-lib/issues/617))
- `FromMetadata` and `ToMetadata` instances for `Contract.Value.CurrencySymbol`

### Removed

- `FromPlutusType` / `ToPlutusType` type classes. ([#464](https://github.com/Plutonomicon/cardano-transaction-lib/pull/464))
- `Contract.Wallet.mkGeroWallet` and `Contract.Wallet.mkNamiWallet` - `Aff` versions should be used instead
- Protocol param update setters for the decentralisation constant (`set_d`) and the extra entropy (`set_extra_entropy`) ([#609](https://github.com/Plutonomicon/cardano-transaction-lib/pull/609))
- `AbsSlot` and related functions have been removed in favour of `Slot`
- Modules `Metadata.Seabug` and `Metadata.Seabug.Share`
- `POST /eval-ex-units` Haskell server endpoint ([#665](https://github.com/Plutonomicon/cardano-transaction-lib/pull/665))
- Truncated test fixtures for time/slots inside `AffInterface` to test time/slots not too far into the future which can be problematic during hardforks https://github.com/Plutonomicon/cardano-transaction-lib/pull/676

### Changed

- Updated `ogmios-datum-cache` - bug fixes ([#542](https://github.com/Plutonomicon/cardano-transaction-lib/pull/542), [#526](https://github.com/Plutonomicon/cardano-transaction-lib/pull/526), [#589](https://github.com/Plutonomicon/cardano-transaction-lib/pull/589))
- Improved error response handling for Ogmios ([#584](https://github.com/Plutonomicon/cardano-transaction-lib/pull/584))
- `balanceAndSignTx` now locks transaction inputs within the current `Contract` context. If the resulting transaction is never used, then the inputs must be freed with `unlockTransactionInputs`
- Updated `ogmios-datum-cache` - bug fixes (#542, #526, #589)
- Made protocol parameters part of `QueryConfig` ([#611](https://github.com/Plutonomicon/cardano-transaction-lib/pull/611))
- Refactored `Plutus.Conversion.Address` code (utilized CSL functionality) ([#609](https://github.com/Plutonomicon/cardano-transaction-lib/pull/609))
- Changed the underlying type of `Slot`, `TransactionIndex` and `CertificateIndex` to `BigNum` ([#609](https://github.com/Plutonomicon/cardano-transaction-lib/pull/609))
- Moved transaction finalization logic to `balanceTx` ([#609](https://github.com/Plutonomicon/cardano-transaction-lib/pull/609))
- Upgraded to CSL v11.0.0-beta.1 ([#609](https://github.com/Plutonomicon/cardano-transaction-lib/pull/609))
- Switched to Ogmios for execution units evaluation ([#665](https://github.com/Plutonomicon/cardano-transaction-lib/pull/665))
- Changed `inputs` inside `TxBody` to be `Set TransactionInput` instead `Array TransactionInput`. This guarantees ordering of inputs inline with Cardano ([#641](https://github.com/Plutonomicon/cardano-transaction-lib/pull/661))
- Upgraded to Ogmios v5.5.0

### Fixed

- Handling of invalid UTF8 byte sequences in the Aeson instance for `TokenName`
- `Types.ScriptLookups.require` function naming caused problems with WebPack ([#593](https://github.com/Plutonomicon/cardano-transaction-lib/pull/593))
- Bad logging in `queryDispatch` that didn't propagate error messages ([#615](https://github.com/Plutonomicon/cardano-transaction-lib/pull/615))
- UTxO Min-Ada-Value calculation ([#611](https://github.com/Plutonomicon/cardano-transaction-lib/pull/611))

## [1.1.0] - 2022-06-30

### Fixed

- Changed `utxoIndex` inside an `UnbalancedTx` to be a `Map` with values `TransactionOutput` instead of `ScriptOutput` so there is no conversion in the balancer to `ScriptOutput`. This means the balancer can spend UTxOs from different wallets instead of just the current wallet and script addresses.

## [1.0.1] - 2022-06-17

### Fixed

- `mustBeSignedBy` now sets the `Ed25519KeyHash` corresponding to the provided `PaymentPubKeyHash` directly. Previously, this constraint would fail as there was no way to provide a matching `PaymentPubKey` as a lookup. Note that this diverges from Plutus as the `paymentPubKey` lookup is always required in that implementation.

## [1.0.0] - 2022-06-10

CTL's initial release!
