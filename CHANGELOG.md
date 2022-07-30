# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and we follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

**Table of Contents**

- [[Unreleased]](#unreleased)
  - [Added](#added)
  - [Changed](#changed)
  - [Fixed](#fixed)
- [[2.0.0-alpha] - 2022-07-06](#200-alpha---2022-07-06)
  - [Added](#added-1)
  - [Removed](#removed)
  - [Changed](#changed-1)
  - [Fixed](#fixed-1)
- [[1.1.0] - 2022-06-30](#110---2022-06-30)
  - [Fixed](#fixed-2)
- [[1.0.1] - 2022-06-17](#101---2022-06-17)
  - [Fixed](#fixed-3)
- [[1.0.0] - 2022-06-10](#100---2022-06-10)

## [Unreleased]

### Added

- `Alt` and `Plus` instances for `Contract`.
- `Contract.Utxos.getUtxo` call to get a single utxo at a given output reference
- `Contract.Monad.withContractEnv` function  that constructs and finalizes a contract environment that is usable inside a bracket callback. **This is the intended way to run multiple contracts**. ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- `Contract.Monad.stopContractEnv` function to finalize a contract environment (close the `WebSockets`). It should be used together with `mkContractEnv`, and is not needed with `withContractEnv`. ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- `Contract.Config` module that contains everything needed to create and manipulate `ConfigParams`, as well as a number of `ConfigParams` fixtures for common use cases. ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- `Contract.Monad.askConfig` and `Contract.Monad.asksConfig` functions to access user-defined configurations. ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- `Contract.Config.WalletSpec` type that allows to define wallet parameters declaratively in `ConfigParams`, instead of initializing wallet and setting it to a `ContractConfig` ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- Faster initialization of `Contract` runtime due to parallelism. ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- `purescriptProject`'s `shell` parameter now accepts `packageLockOnly`, which if set to true will stop npm from generating node\_modules. This is enabled for CTL developers
- `Contract.Transaction.awaitTxConfirmed` and `Contract.Transaction.awaitTxConfirmedWithTimeout`
- `Contract.TextEnvelope.textEnvelopeBytes` and family to decode the `TextEnvelope` format, a common format output by tools like `cardano-cli` to serialize values such as cryptographical keys and on-chain scripts
- `Contract.Wallet.isNamiAvailable` and `Contract.Wallet.isGeroAvailable` functions ([#558](https://github.com/Plutonomicon/cardano-transaction-lib/issues/558)])
- `Contract.Transaction.balanceTxWithOwnAddress` and `Contract.Transaction.balanceTxsWithOwnAddress` to override an `Address` used in `balanceTx` internally ([#775](https://github.com/Plutonomicon/cardano-transaction-lib/pull/775))
- `Contract.Transaction.awaitTxConfirmedWithTimeoutSlots` waits a specified number of slots for a transaction to succeed. ([#790](https://github.com/Plutonomicon/cardano-transaction-lib/pull/790))

### Changed

- `runContract` now accepts `ConfigParams` instead of `ContractConfig` ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- `mkContractConfig` has been renamed to `mkContractEnv`. Users are advised to use `withContractEnv` instead to ensure proper finalization of WebSocket connections. ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- CTL's `overlay` no longer requires an explicitly passed `system`
- Switched to CSL for utxo min ada value calculation ([#715](https://github.com/Plutonomicon/cardano-transaction-lib/pull/715))
- `ConfigParams` is now a type synonym instead of a newtype. `ContractConfig` has been renamed to `ContractEnv`.
- Moved logging functions to `Contract.Log` from `Contract.Monad` ([#727](https://github.com/Plutonomicon/cardano-transaction-lib/issues/727)
- Renamed `Contract.Wallet.mkKeyWalletFromPrivateKey` to `Contract.Wallet.mkKeyWalletFromPrivateKeys`.

### Removed

- `Contract.Monad.traceTestnetContractConfig` - use `Contract.Config.testnetNamiConfig` instead.
- `runContract_` - use `void <<< runContract`
- Examples now wait for transactions to be confirmed and log success.
- ServerConfig accepts a url `path` field

### Fixed

- Bug with collateral selection: only the first UTxO provided by wallet was included as collateral [(#723)](https://github.com/Plutonomicon/cardano-transaction-lib/issues/723)
- Bug with collateral selection for `KeyWallet` when signing multiple transactions ([#709](https://github.com/Plutonomicon/cardano-transaction-lib/pull/709))
- Properly implemented CIP-25 V2 metadata. Now there's no need to split arbitrary-length strings manually to fit them in 64 PlutusData bytes (CTL handles that). A new `Cip25String` type has been introduced (a smart constructor ensures that byte representation fits 64 bytes, as required by the spec). Additionally, a new `Metadata.Cip25.Common.Cip25TokenName` wrapper over `TokenName` is added to ensure proper encoding of `asset_name`s. There are still some minor differences from the spec:

-- We do not split strings in pieces when encoding to JSON
-- We require a `"version": 2` tag.
-- `policy_id` must be 28 bytes
-- `asset_name` is up to 32 bytes.

See https://github.com/cardano-foundation/CIPs/issues/303 for motivation
- `ogmios-datum-cache` now works on `x86_64-darwin`

## [2.0.0-alpha] - 2022-07-05

This release adds support for running CTL contracts against Babbage-era nodes. **Note**: this release does not support Babbagge-era features and improvements, e.g. inline datums and reference inputs. Those feature will be implemented in v2.0.0 proper.

### Added

- `withKeyWallet` utility that allows to simulate multiple actors in Plutip environment ([#663](https://github.com/Plutonomicon/cardano-transaction-lib/issues/663))
- Support for using a `PrivateKey` as a `Wallet`.
- `mkKeyWalletFromFile` helper to use `cardano-cli`-style `skey`s
- Single `Plutus.Conversion` module exposing all `(Type <-> Plutus Type)` conversion functions ([#464](https://github.com/Plutonomicon/cardano-transaction-lib/pull/464))
- Support for using a `PrivateKey` as a `Wallet`
- `logAeson` family of functions to be able to log JSON representations
- `EncodeAeson` instances for most types under `Cardano.Types.*` as well as other useful types (`Value`, `Coin`, etc.)
- `getProtocolParameters` call to retrieve current protocol parameters from Ogmios ([#541](https://github.com/Plutonomicon/cardano-transaction-lib/issues/541))
- `Contract.Utxos.getWalletBalance` call to get all available assets as a single `Value` ([#590](https://github.com/Plutonomicon/cardano-transaction-lib/issues/590))
- `balanceAndSignTxs` balances and signs multiple transactions while taking care to use transaction inputs only once
- Ability to load stake keys from files when using `KeyWallet` ([#635](https://github.com/Plutonomicon/cardano-transaction-lib/issues/635))
- Implement utxosAt for `KeyWallet` ([#617](https://github.com/Plutonomicon/cardano-transaction-lib/issues/617))
- `FromMetadata` and `ToMetadata` instances for `Contract.Value.CurrencySymbol`
- `Contract.Chain.waitUntilSlot` to delay contract execution until local chain tip reaches certain point of time (in slots).

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
- `balanceAndSignTx` now locks transaction inputs within the current `Contract` context. If the resulting transaction is never used, then the inputs must be freed with `unlockTransactionInputs`.
- Updated `ogmios-datum-cache` - bug fixes (#542, #526, #589).
- Made protocol parameters part of `QueryConfig`.
- Refactored `Plutus.Conversion.Address` code (utilized CSL functionality).
- Changed the underlying type of `Slot`, `TransactionIndex` and `CertificateIndex` to `BigNum`.
- Moved transaction finalization logic to `balanceTx`.
- Upgraded to CSL v11.0.0-beta.1.
- `purescriptProject` (exposed via the CTL overlay) was reworked significantly. Please see the [updated example](./doc/ctl-as-dependency#using-the-ctl-overlay) in the documentation for more details.
- Switched to Ogmios for execution units evaluation ([#665](https://github.com/Plutonomicon/cardano-transaction-lib/pull/665))
- Changed `inputs` inside `TxBody` to be `Set TransactionInput` instead `Array TransactionInput`. This guarantees ordering of inputs inline with Cardano ([#641](https://github.com/Plutonomicon/cardano-transaction-lib/pull/661))
- Upgraded to Ogmios v5.5.0

### Fixed

- Handling of invalid UTF8 byte sequences in the Aeson instance for `TokenName`
- `Types.ScriptLookups.require` function naming caused problems with WebPack ([#593](https://github.com/Plutonomicon/cardano-transaction-lib/pull/593))
- Bad logging in `queryDispatch` that didn't propagate error messages ([#615](https://github.com/Plutonomicon/cardano-transaction-lib/pull/615))
- Utxo min ada value calculation ([#611](https://github.com/Plutonomicon/cardano-transaction-lib/pull/611))
- Discarding invalid inputs in `txInsValues` instead of yielding an error ([#696](https://github.com/Plutonomicon/cardano-transaction-lib/pull/696))
- Locking transaction inputs before the actual balancing of the transaction ([#696](https://github.com/Plutonomicon/cardano-transaction-lib/pull/696))

## [1.1.0] - 2022-06-30

### Fixed

- Changed `utxoIndex` inside an `UnbalancedTx` to be a `Map` with values `TransactionOutput` instead of `ScriptOutput` so there is no conversion in the balancer to `ScriptOutput`. This means the balancer can spend UTxOs from different wallets instead of just the current wallet and script addresses.

## [1.0.1] - 2022-06-17

### Fixed

- `mustBeSignedBy` now sets the `Ed25519KeyHash` corresponding to the provided `PaymentPubKeyHash` directly. Previously, this constraint would fail as there was no way to provide a matching `PaymentPubKey` as a lookup. Note that this diverges from Plutus as the `paymentPubKey` lookup is always required in that implementation.

## [1.0.0] - 2022-06-10

CTL's initial release!
