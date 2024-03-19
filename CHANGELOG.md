# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and we follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [[Unreleased]](#unreleased)
  - [Added](#added)
  - [Changed](#changed)
  - [Fixed](#fixed)
  - [Removed](#removed)
- [[v7.0.0]](#v700)
  - [Added](#added-1)
  - [Changed](#changed-1)
  - [Fixed](#fixed-1)
  - [Removed](#removed-1)
- [[v6.0.0]](#v600)
  - [Added](#added-2)
  - [Changed](#changed-2)
  - [Fixed](#fixed-2)
  - [Removed](#removed-2)
- [[v5.0.0]](#v500)
  - [Added](#added-3)
  - [Changed](#changed-3)
  - [Removed](#removed-3)
  - [Fixed](#fixed-3)
  - [Runtime Dependencies](#runtime-dependencies)
- [[v4.0.2] - 2023-01-17](#v402---2023-01-17)
  - [Fixed](#fixed-4)
- [[v4.0.1] - 2022-12-20](#v401---2022-12-20)
  - [Added](#added-4)
- [[v4.0.0] - 2022-12-15](#v400---2022-12-15)
  - [Added](#added-5)
  - [Changed](#changed-4)
  - [Removed](#removed-4)
  - [Fixed](#fixed-5)
  - [Runtime Dependencies](#runtime-dependencies-1)
- [[3.0.0] - 2022-11-21](#300---2022-11-21)
  - [Added](#added-6)
  - [Changed](#changed-5)
  - [Removed](#removed-5)
  - [Fixed](#fixed-6)
  - [Runtime Dependencies](#runtime-dependencies-2)
- [[2.0.0] - 2022-09-12](#200---2022-09-12)
  - [Added](#added-7)
  - [Changed](#changed-6)
  - [Removed](#removed-6)
  - [Fixed](#fixed-7)
- [[2.0.0-alpha] - 2022-07-05](#200-alpha---2022-07-05)
  - [Added](#added-8)
  - [Removed](#removed-7)
  - [Changed](#changed-7)
  - [Fixed](#fixed-8)
- [[1.1.0] - 2022-06-30](#110---2022-06-30)
  - [Fixed](#fixed-9)
- [[1.0.1] - 2022-06-17](#101---2022-06-17)
  - [Fixed](#fixed-10)
- [[1.0.0] - 2022-06-10](#100---2022-06-10)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## [Unreleased]

### Added

- Sharing wallets between Plutip tests - see [the docs for this feature](./doc/plutip-testing.md#sharing-wallet-state-between-tests) ([#1585](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1585))
  - `runPlutipTestPlan` is a new function that executes a `ContractTestPlan`.
  - `sameWallets` is a new function that creates a `ContractTestPlan` from a `UtxoDistribution` and a `TestPlanM` of the same wallets running different `Contract`s.

### Changed

- Replaced custom CIP-30 wrapper code with [`purescript-cip30-typesafe`](https://github.com/mlabs-haskell/purescript-cip30-typesafe/) - ([#1583](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1583))

### Fixed

- WebAssembly memory leaks (`csl-gc-wrapper` used to depend on unstable `wasm-bidngen` API [that got changed](https://github.com/mlabs-haskell/csl-gc-wrapper/commit/2dea38228b77f7c904aafef12eece7e5af195977)) ([#1595](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1595))

### Removed

- **[IMPORTANT]** Removed use of conditional code rewriting based on `BROWSER_RUNTIME` env variable during bundling ([#1595](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1598)). This change simplifies the bundling process, but it requires a number of updates for all CTL-dependent projects:

   * WebPack users should make this change to the webpack config:
   ```diff
       plugins: [
   -      new webpack.DefinePlugin({
   -        BROWSER_RUNTIME: isBrowser
   -      }),
   ```
   * Esbuild users should make this change:
   ```diff
   const config = {
     ...
   -    define: {
   -      BROWSER_RUNTIME: isBrowser ? "true" : '""'
   -    },
   ```
   * All users should update the runtime dependencies:
   ```diff
   -    "@emurgo/cardano-message-signing-browser": "1.0.1",
   -    "@emurgo/cardano-message-signing-nodejs": "1.0.1",
   +    "@mlabs-haskell/cardano-message-signing": "1.0.1",
   -    "apply-args-browser": "0.0.1",
   -    "apply-args-nodejs": "0.0.1",
   +    "@mlabs-haskell/uplc-apply-args": "1.0.0",
   +    "isomorphic-ws": "^5.0.0",
   -    "ws": "8.4.0",
   +    "ws": "^8.16.0",
   +    "web-encoding": "^1.1.5",
   ```

- `ModifyTx` error: made conversion functions total and removed the need to handle it

## [v7.0.0]

### Added

- [esbuild](https://esbuild.github.io/) bundler support.
  - To update your package to use esbuild, add new `devDependencies` to your `package.json`:

```diff
+    "esbuild": "0.18.11",
+    "esbuild-plugin-polyfill-node": "^0.3.0",
+    "esbuild-plugin-wasm": "^1.1.0",
```

Then consult with [the template's build scripts](./templates/ctl-scaffold/esbuild/) - also see the new [Makefile](./templates/ctl-scaffold/Makefile) setup and [NPM scripts](./templates/ctl-scaffold/package.json).

### Changed

- PureScript compiler version has been updated to v0.15.8. ([#1521](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1521))
  - PureScript v0.15.x outputs and expects ES modules (instead of CommonJS), which means that FFI code in dependent projects should be updated to use [ES module-style imports and exports](https://nodejs.org/api/esm.html), and consumers of CTL-based bundles should expect ES modules as well.
  - Due to limitations of ES modules (inability to patch at runtime), CTL now uses vendored versions of CSL for node and the browser:

```diff
-    "@emurgo/cardano-serialization-lib-browser": "11.2.1",
-    "@emurgo/cardano-serialization-lib-nodejs": "11.2.1",
+    "@mlabs-haskell/cardano-serialization-lib-gc-browser": "^1.0.6",
+    "@mlabs-haskell/cardano-serialization-lib-gc-nodejs": "^1.0.6",
```

  - Our vendored version of `json-bigint` should be updated:

```diff
-    "@mlabs-haskell/json-bigint": "1.0.0",
+    "@mlabs-haskell/json-bigint": "2.0.0",
```

  - `utf-8-validate` NPM package has been added (used during bundling):

```diff
+    "utf-8-validate": "^5.0.10",
```

- WebPack bundling machinery updates:

```diff
-    "webpack": "5.67.0",
-    "webpack-cli": "4.10",
-    "webpack-dev-server": "4.7.4"
+    "webpack": "5.88.1",
+    "webpack-cli": "5.1.4",
+    "webpack-dev-server": "4.15.1"
```

- Nix machinery refactorings ([#1521](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1521)):
  - `buildPursDependencies` is a new function that allows to build everything except the source files of a project itself. It allows to skip rebuilding all the dependencies when using Nix every time. `buildPursProject` calls `buildPursDependencies` automatically.
  - `censorCodes` and `strictComp` arguments have been removed from `purescriptProject` and added to `buildPursDependencies` and `buildPursProject` for more granular control.
  - `runPursTest`: new `psEntryPoint` argument, for PureScript-level entry point function ("main" by default).
  - `runE2ETest`: new `runnerMain` and `runnerPsEntryPoint` arguments to control which module should be the runner.
  - `runE2ETest`: new arguments for configuring the E2E test suite: `envFile`, `emptySettingsFile` and `testTimeout`.
  - `bundlePursProjectEsbuild` is a bundling function that uses newly introduced `esbuild`.
  - `bundlePursProject` is renamed to `bundlePursProjectWebpack`
- `Data.BigInt` (`purescript-bigints`) dependency was replaced with `JS.BigInt` (`purescript-js-bigints`), that uses native JavaScript BigInt instead of `BigInteger.js` library. You can remove `big-integer` NPM dependency from your project, if you don't use it elsewhere. ([#1551](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1551))

### Fixed

### Removed

- Temporarily removed `buildSearchablePursDocs` and `launchSearchablePursDocs` from nix machinery - see [#1578](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1578) for context. Use `buildPursDocs` for now. ([#1521](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1521))

## [v6.0.0]

### Added

- `mkUnbalancedTxE`, `balanceTxE` and `balanceTxWithConstraintsE` as
  non-throwing versions of `mkUnbalancedTx`, `balanceTx` and
  `balanceTxWithConstraints` ([#1545](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1545)
- `explainMkUnbalancedTxError` and `explainBalanceTxError`, which prettyprint
  `MkUnbalancedTxError` and `BalanceTxError` for a more human-readable output. ([#1545](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1545)
- `Contract.Time.getCurrentEra` and `Contract.Time.normalizeTimeInterval`,
  providing an improved interface for eras and time ranges
  ([#1542](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1542)).
- Added `extraSources` and `data` features to CTL's Nix build function ([#1516](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1516))
- Added several `Ring`-like numeric instances for `Coin` ([#1485](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1485))
- Added `ToData` and `FromData` instances for `PoolPubKeyHash` ([#1483](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1483))
- **[IMPORTANT]** New machinery to achieve better synchronization between wallets and query layer has been added. This affects all CTL-based apps when light wallet browser extensions are in use. See [here](./doc/query-layers.md) for more info ([#1440](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1440))
- **Local Blockfrost runtime** based on [run-your-own version of Blockfrost](https://github.com/blockfrost/blockfrost-backend-ryo/) - see [here](./doc/blockfrost.md#running-blockfrost-locally) for more info ([#1395](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1395))
- **Lace wallet support** - ([#1477](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1477))
- Automatic retries for `503 Service Unavailable` Kupo request errors. Retry attempts happen with exponentially increasing intervals ([#1436](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1436))
- New functions in the assertion library to track changes in CIP-30 wallet total balance - see `Contract.Test.Assert` ([#1440](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1440))
- Added `start-runtime` npm script to the template, to simplify UX ([#1440](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1440))
- Configuration options for Kupo in `buildCtlRuntime` ([`deferDbIndexes`](https://cardanosolutions.github.io/kupo/#section/Getting-started/-defer-db-indexes) and [`pruneUtxo`](https://cardanosolutions.github.io/kupo/#section/Getting-started/-prune-utxo)) ([#1448](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1448))
- `Contract.JsSdk` module containing helper functions for developers who want to use CTL from JS. See also: [this new guide](./doc/using-from-js.md) ([#1453](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1453))
- New [docs for contract environment](./doc/contract-environment.md) ([#1453](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1453))
- Cluster configuration options to `PlutipConfig`: `epochSize`, `maxTxSize` and `raiseExUnitsToMax` - see [Plutip docs](./doc/plutip-testing.md) ([#1494](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1494))
- [HD wallet support](./doc/key-management.md) with mnemonic seed phrases ([#1498](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1498))
- Ogmios-specific functions for Local TX Monitor Ouroboros Mini-Protocol in `Contract.Backend.Ogmios` ([#1508](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1508/))
- New `mustSendChangeWithDatum` balancer constraint that adds datum to all change outputs ([#1510](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1510/))
- Support for generic CIP-30 wallets by name ([#1524](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1524))
- Full additional utxos support for Blockfrost backend ([#1537](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1537))
- New `submitTxE`, an error returning variant of `submitTx`
- Allow providing a custom set of UTxOs for collateral selection, overriding the wallet (`mustUseCollateralUtxos` balancer constraint) ([#1513](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1513))

### Changed

- **[IMPORTANT]** Ogmios has been updated and it must be started with `--include-transaction-cbor`. CTL relies on CBOR variants being available in the responses.
- **[IMPORTANT]** It is no more recommended to use `utxosAt` to get UTxOs at light wallet addresses. It may be a source of application bugs in some cases due to how wallets operate. Please see *Synchronization and wallet UTxO locking* section [here](./doc/query-layers.md) ([#1440](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1440))
- `scriptCurrencySymbol` no longer returns `Maybe`
  ([#1538](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1538)
- Slot to/from POSIXTime conversion functions now live outside of `Effect` ([#1490](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1490))
- All uses of `utxosAt` call have been replaced with `getWalletUtxos` in the balancer ([#1440](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1440))
- Naming changes in `Contract.Test.Assert` for consistency with other functions ([#1440](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1440)):
  - `checkNewUtxosAtAddress` -> `assertNewUtxosAtAddress`
  - `assertLovelaceDeltaAtAddress` -> `checkLovelaceDeltaAtAddress`
  - `assertValueDeltaAtAddress` -> `checkValueDeltaAtAddress`
- New fields added to `ContractParams`: `ContractTimeParams` and `ContractSynchronizationParams`. Default values are `Contract.Config.defaultTimeParams` and `Contract.Config.defaultSynchronizationParams` . See [docs on query layers configuration](./doc/query-layers.md) for more info. ([#1440](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1440))
- `getWalletBalance` is now implemented via `getWalletUtxos` ([#1440](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1440))
- `PoolPubKeyHash` is now a wrapper over `PubKeyHash` instead of `Ed25519KeyHash` ([#1440](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1440))
- `Contract.Wallet.Key.publicKeyFromPrivateKey` uses `PublicKey` type from public API ([#1440](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1440))
- `Contract.Test.Blockfrost.executeContractTestsWithBlockfrost` does not require optional `CtlBackendParams` value (it is now constructed from environment variables).
- `plutip-server` has been moved from [Plutip repo](https://github.com/mlabs-haskell/plutip) to CTL ([#1415](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1415))
- `UnattachedUnbalancedTx` has been renamed and moved to `Contract.UnbalancedTx.UnbalancedTx`, the old `UnbalancedTx` type has been removed as not needed. `mkUnbalancedTx` function has been moved to `Contract.UnbalancedTx`  ([#1462](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1462))
- Default NodeJS stable version is now v18 ([#1453](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1453))
- Do not require light wallet collateral for all interactions ([#1477](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1477))
- Removed re-exports of wallet-related functions from `Contract.Utxos` and `Contract.Address` (use `Contract.Wallet`) ([#1477](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1477))
- `ownPaymentPubKeysHashes` renamed to `ownPaymentPubKeyHashes`, `ownStakePubKeysHashes` renamed to `ownStakePubKeyHashes` and both moved to `Contract.Wallet` ([#1477](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1477))
- `runContractTestsWithKeyDir` now exposed from `Contract.Test`
  ([#1549](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1549))
- Pretty-printing improvements: UTxO lists and combined input/output/mint/fee values are now being pretty-printed instead `Show`n in the balancer ([#1531](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1531))
- `mkUnbalancedTx`, `balanceTx` and `balanceTxWithConstraints` now throw instead
  of returning in `Either` ([#1545](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1545))
- `mkUnbalancedTx` isn't exported from `Contract.ScriptLookups` anymore; get it
  from `Contract.UnbalancedTx` instead.
  ([#1545](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1545))
- Types changed: `TxEvaluationFailure`, `PoolParametersR`, due to ogmios update ([#1532](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1532)).
- Updated versions of `ctl-runtime` services: Ogmios to 6.0.0, Blockfrost to v1.7.0.

### Fixed

- `UseMnemonic` and `ConnectToGenericCip30` constructors now exported from
  `Contract.Config`
  ([#1546])(https://github.com/Plutonomicon/cardano-transaction-lib/pull/1546))
- `waitUntilSlot` can now be used with a slot far in the future ([#1490](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1490))
- `Unable to convert Slot to POSIXTime` error of `waitUntilSlot` doesn't omit important error details anymore ([#1458](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1458))
- Performance issues when using Eternl in multi-address mode ([#1440](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1440))
- `ConnectToNuFi` now reexported in `Contract.Wallet` ([#1435](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1435))
- Fix a bug in UTxO selection in `Cip30Mock` (that affected `Cip30Mock` users) ([1437](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1437))
- Fixed `bundlePursProject` crashing if build output directory exists ([#1438](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1438))
- Better reporting for WebSocket errors  ([#1403](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1403))
- `Contract.Test.Assert`: handle exceptions coming from callbacks that calculate expected values  (in `checkLovelaceDeltaAtAddress`, `checkTokenDeltaAtAddress`, `checkLovelaceDeltaInWallet` and `checkTokenDeltaInWallet`), propagate original error messages correctly ([#1440](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1440))
- Fixed collateral selection - sort the UTxOs by ADA value in descending order, before selecting them for collateral, to ensure that if a combination that satisfies the requirements is possible, it will be selected. The bug affected CIP-30 wallets. ([#1440](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1440))
- Output correct reward address in CIP-30 mock ([#1440](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1440))
- Add a single-slot wait at Plutip startup before attempting to query any wallet UTxOs ([#1470](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1470))
- Index `Reward` redeemers properly ([#1419](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1419),  [#1462](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1462))
- A problem with collateral selection not respecting `mustNotSpendUtxosWithOutRefs` ([#1509](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1509))
- A problem with too many change UTxOs being generated ([#1530](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1530))
- A problem where tx evaluation with additional utxos failed with an Ogmios `AdditionalUtxoOverlap` exception if some additional utxos got confirmed in the meantime ([#1537](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1537))
- `Contract.PlutusData.redeemerHash` definition ([#1565](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1565))

### Removed

- `mkUnbalancedTxM` and `balanceTxM`
  ([#1547](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1547))
- `E2E_SKIP_JQUERY_DOWNLOAD` configuration variable for [E2E test suite](./doc/e2e-testing.md). It is not needed, because it's expected value can be determined from the environment, and thus it can be an implementation detail ([#1440](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1440))
- `reindexSpentScriptRedeemers` function from the public API - if there is a need to modify the `Transaction` in a way that breaks redeemer indices, it should be done before balancing ([#1462](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1462))
- Typed scripts and constraints interface. In practice, it means that the following types now have no type-level arguments: `TxConstraints`, `ScriptLookups`.
- Removed error variants (no more needed) ([#1545](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1545)):
  - `ImpossibleError`
  - `CannotConvertPaymentPubKeyHash`, `CannotHashMintingPolicy`, `CannotHashValidator` and `CannotHashDatum` variants of `MkUnbalancedTxError`
  - `InvalidInContext`
  - `CannotGetBigIntFromNumber'` and `CannotGetBigNumFromBigInt'` variants of `PosixTimeToSlotError`

## [v5.0.0]

### Added

- **Blockfrost support** - see [`blockfrost.md`](./doc/blockfrost.md) ([#1260](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1260))
- A test runner interface for Blockfrost (`Contract.Test.Blockfrost`). See [`blockfrost.md`](./doc/blockfrost.md) ([#1420](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1420))
- `blake2b224Hash` and `blake2b224HashHex` functions for computing blake2b-224 hashes of arbitrary byte arrays ([#1323](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1323))
- `bundlePursProject` allows passing of `includeBundledModule` flag to export the bundled JS module `spago bundle-module` outputs
- `Contract.Transaction` exports `mkPoolPubKeyHash` and `poolPubKeyHashToBech32` for bech32 roundtripping ([#1360](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1360))
- `Contract.isTxConfirmed` function to check if a transaction is confirmed at the moment.

### Changed

- **Contract interface change:** `Contract` does not have a row type parameter anymore. Use `ReaderT` or pass values explicitly to access them during runtime.
- **Contract interface change:** `ConfigParams r` is replaced by `ContractParams` with the same purpose.
- `SystemStart` now has `DateTime` (rather than `String`) as the underlying type ([#1377](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1377))
- `EraSummaries` now does not have an `EncodeAeson` instance. Consider wrapping it in `OgmiosEraSummaries` for Aeson encoding. ([#1377](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1377))
- Testing interface is re-implemented. Assertion functions from `Contract.Test.Utils` are moved to `Contract.Test.Assert`. See [the docs](./doc/test-utils.md) for info on the new interface. ([#1389](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1389))
- Balancer no longer selects UTxOs which use PlutusV2 features when the transaction contains PlutusV1 scripts ([#1349](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1349))
- `startPlutipCluster` error message now includes cluster startup failure details. ([#1407](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1407))
- `PlutipTest` is now known as `Contract.Test.ContractTest`. It has been semantically untied from Plutip, because we now have another test runner for tests that rely on particular funds distributions - [Blockfrost](./doc/blockfrost.md). See `Contract.Test.Blockfrost.runContractTestsWithBlockfrost` ([#1260](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1260))
- `Contract.Staking.getPoolParameters` has been moved to `Contract.Backend.Ogmios.getPoolParameters`. This function only runs with Ogmios backend, because Blockfrost [does not provide](https://github.com/blockfrost/blockfrost-backend-ryo/issues/82) all the required values ([#1260](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1260))
- Use of [CIP-40](https://cips.cardano.org/cip/CIP-0040/) collateral output is now enabled with CIP-30 wallets ([#1260](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1260)).
- `reindexSpentScriptRedeemers` is no longer in Contract (it's pure) ([#1260](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1260))

### Removed

- **Important** [Ogmios Datum Cache](https://github.com/mlabs-haskell/ogmios-datum-cache) is no longer a runtime dependency of CTL, as well as its Postgres DB - if updating, remove them from your runtime.

### Fixed
- CIP-25 strings are now being split into chunks whose sizes are less than or equal to 64 to adhere to the CIP-25 standard ([#1343](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1343))
- Critical upstream fix in [`purescript-bignumber`](https://github.com/mlabs-haskell/purescript-bignumber/pull/2)
- `OutputDatum` aeson encoding now roundtrips ([#1388](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1388))
- Fix incorrect redeemer indexing for Plutus stake validator scripts ([#1417](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1417))

### Runtime Dependencies

- [Ogmios](https://github.com/mlabs-haskell/ogmios) - v5.5.7
- [Kupo](https://github.com/CardanoSolutions/kupo) - v2.2.0
- [Cardano-Node](https://github.com/input-output-hk/cardano-node/) - v1.35.4
- [Plutip](https://github.com/mlabs-haskell/plutip) - commit 8d1795d9ac3f9c6f31381104b25c71576eeba009

## [v4.0.2] - 2023-01-17

### Fixed

- Fixed nix build issue on MacOS systems

## [v4.0.1] - 2022-12-20

### Added

- Added a SECP256k1 explainer document ([#1346](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1346))

## [v4.0.0] - 2022-12-15

### Added

- NuFi wallet support ([#1265](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1265))
- Add `submitTxFromConstraints` and `submitTxFromConstraintsReturningFee` in `Contract.Transaction`. `submitTxFromConstraints` builds a transaction that satisfies the constraints, then submits it to the network. It is analogous to `submitTxConstraintsWith` function in Plutus and replaces `Helpers.buildBalanceSignAndSubmitTx`.
- Support for CIP-49 crypto primitives: SECP256k1 [ECDSA](./src/Contract/Crypto/Secp256k1/ECDSA.purs) and [Schnorr](./src/Contract/Crypto/Secp256k1/Schnorr.purs) (verification functions, signing and key generation) ([1273](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1273))

### Changed

- Running plutip servers attaches on SIGINT handlers and therefore node will not exit by default. ([#1231](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1231)).
- `TestPlanM`, `interpret` and `interpretWithConfig` are now public in `Contract.Test.Mote` and our custom `consoleReporter` in `Contract.Test.Mote.ConsoleReporter`. ([#1261](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1261)).
- Internal datum conversions are now total, resulting in some datum-related Contract functions dropping the use of `Maybe`, for example `datumHash`, `convertPlutusData` and their related functions. ([#1284](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1284)).
- CIP-25 `policy_id` and `asset_name` metadata keys no longer include a `0x` prefix for compatibility with Blockfrost ([#1309](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1309)).
- `purescript-aeson` package has been updated:
  - the performance has generally been improved
  - `encodeAeson'` is now `encodeAeson` (and it returns just `Aeson` instead of an `AesonEncoder`)
  - `Number` type is not supported anymore (due to `NaN` and `+/-Infinity`) - use `Aeson.finiteNumber` function

### Removed

- `ctl-server`, a haskell binary that was providing ability to apply arguments to parametrized scripts, was replaced by an implementation that uses WASM library (`Contract.Scripts.applyArgs`) ([#483](https://github.com/Plutonomicon/cardano-transaction-lib/issues/483))

### Fixed

- Added missing `stakePoolTargetNum` ("`nOpt`") protocol parameter (see [CIP-9](https://cips.cardano.org/cip/CIP-0009)) ([#571](https://github.com/Plutonomicon/cardano-transaction-lib/issues/571))
- CIP-30 `signData` response handling ([#1289](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1289))

### Runtime Dependencies

- [Ogmios](https://github.com/mlabs-haskell/ogmios) - v5.5.7
- [Kupo](https://github.com/CardanoSolutions/kupo) - v2.2.0
- [Cardano-Node](https://github.com/input-output-hk/cardano-node/) - v1.35.4
- [Ogmios-Datum-Cache](https://github.com/mlabs-haskell/ogmios-datum-cache) - commit 862c6bfcb6110b8fe816e26b3bba105dfb492b24
- [Plutip](https://github.com/mlabs-haskell/plutip) - commit 8d1795d9ac3f9c6f31381104b25c71576eeba009

## [3.0.0] - 2022-11-21

### Added

- Support passing the inital UTxO distribution as an Array and also get the KeyWallets as an Array when writing Plutip tests. ([#1018](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1018)). An usage example can be found [here](doc/plutip-testing.md).
- New `Contract.Test.Utils` assertions and checks: `assertOutputHasRefScript`, `checkOutputHasRefScript`, `checkTxHasMetadata` ([#1044](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1044))
- `Parallel` instance to `Contract` monad. Parallel capabilities are in the associated `ParContract` datatype ([#1037](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1037))
- Balancer constraints interface (check [Building and submitting transactions](https://github.com/Plutonomicon/cardano-transaction-lib/blob/95bdd213eff16a5e00df82fb27bbe2479e8b4196/doc/getting-started.md#building-and-submitting-transactions) and `examples/BalanceTxConstraints.purs` for reference) ([#1053](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1053))
- New `Contract.Transaction` functions accepting balancer constraints as a parameter: `balanceTxWithConstraints`, `balanceTxsWithConstraints`, `withBalancedTxWithConstraints`, `withBalancedTxsWithConstraints` ([#1053](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1053))
- New functions `addressWithNetworkTagFromBech32` and `addressFromBech32` in `Contract.Address`, the second checking that address network Id corresponds to the contract environment network Id. ([#1062](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1062))
- `Contract.CborBytes` for CBOR-related functionality. ([#850](https://github.com/Plutonomicon/cardano-transaction-lib/issues/850))
- `ToData` & `FromData` instances for `PublicKey` in `Cardano.Types.Transaction` ([#998](https://github.com/Plutonomicon/cardano-transaction-lib/issues/998))
- `Contract.Keys` module that exposes smart constructors for `PublicKey` & `Ed25519Signature`, namely: `mkEd25519Signature`, `mkPubKey`.
- `Contract.createAdditionalUtxos` to build an expected utxo set from transaction outputs, useful for transaction chaining ([#1046](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1046))
- `DecodeAeson` instance for `NativeScript` data type ([#1069](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1069)).
- `Contract.Wallet` exports `mkWalletBySpec` ([#1157](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1157))
- `ctl-server` NixOS module ([#1194](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1194)). See [nix/test-nixos-configuration.nix](nix/test-nixos-configuration.nix) for example usage and [nix/ctl-server-nixos-module.nix](https://github.com/Plutonomicon/cardano-transaction-lib/blob/43b31399ac743be385b7ee38066a794e5c3c199c/nix/ctl-server-nixos-module.nix).
- Ability to run E2E tests on private Plutip testnets using CIP-30 wallet mock - see [the docs](./doc/e2e-testing.md#using-cip-30-mock-with-plutip) ([#1166](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1166))
- `Contract.Plutarch.Types` module with `PRational` type which is a newtype of Rational with `ToData` and `FromData` instance which are compatible with Plutarch ([#1221](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1221))
- New constraints for stake operations ([#1060](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1060)):
  - Pool registration (`mustRegisterPool`)
  - Pool retirement (`mustRetirePool`)
  - Stake credential registration (`mustRegisterStakePubKey`, `mustRegisterStakeScript`)
  - Stake delegation (`mustDelegateStakeNativeScript`, `mustDelegateStakePlutusScript`, `mustDelegateStakePubKey`)
  - Staking rewards withdrawal (`mustWithdrawStakePubKey`, `mustWithdrawStakePlutusScript`, `mustWithdrawStakeNativeScript`)
  - Stake credential deregistration (`mustDeregisterStakePubKey`, `mustDeregisterStakePlutusScript`, `mustDeregisterStakeNativeScript`)
- New query layer functions to retrieve staking-related info from Ogmios ([#1060](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1060)):
  - `Contract.Staking.getPoolIds`
  - `Contract.Staking.getPoolParameters`
  - `Contract.Staking.getPubKeyHashDelegationsAndRewards`
  - `Contract.Staking.getValidatorHashDelegationsAndRewards`
- `Contract.Test.Plutip.testPlutipContracts` to run multiple Mote Plutip tests on the same Plutip cluster, saving on cluster startup time. ([#1154](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1154)).
- `EncodeAeson` and `DecodeAeson` instances for `TransactionInput`, `TransactionOutput`/`TransactionOutputWithRefScript` and `PaymentPubKeyHash`([#1138](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1138))
- `mustSendChangeToAddress` balancer constraint, allowing to explicitly set the address to send all generated change to ([#1243](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1243))
- `mustUseUtxosAtAddress` and `mustUseUtxosAtAddresses` balancer constraints, allowing to specify addresses that should be treated like utxos sources during balancing ([#1243](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1243))
- Add ability to provide extra browser CLI arguments in E2E test suite ([#1253](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1253))
- The configuration option for plutip clusters: slot length can be adjusted using `clusterConfig` field of `PlutipConfig`. epoch size must currently remain at `80` however due to ([#1272](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1272))

### Changed

- Bumped cardano-serialization-lib dependencies to version 11.1.1-alpha.1 ([#1163](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1163))
- `Contract.Transaction.calculateMinFee` and `Contract.Transaction.calculateMinFeeM` now accept additional UTxOs.
- Reorganised the library into new namespaces. Namely: library internals, tests, and examples are now under `Ctl.Internal.*`, `Test.Ctl.*`, and `Ctl.Examples.*` respectively. Documentation and comments have been updated to use these new names, but not entries of previous releases in the changelog. ([#1039](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1039))
- Switched to `preview` testnet by default ([#1030](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1030))
- `addressFromBech32` checks that address network Id corresponds to the contract environment Id and is therefore lifted to the `Contract` monad ([#1062](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1062))
- `keyWalletPrivatePaymentKey` and `keyWalletPrivateStakeKey` are now in to the public `Contract.Wallet.Key` API. ([#1094](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1094))
- Completely new E2E (headless browser) test suite. It is now both easier to set up and use, and less flaky. The shell script has been removed (and re-implemented in PureScript). The suite can now be configured using environment variables (`test/e2e.env`) or CLI arguments. It is no more required to provide an accompanying tester script for each of the `Contract`s to be tested. ([#1058](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1058), [#986](https://github.com/Plutonomicon/cardano-transaction-lib/issues/986))
- The `logLevel` from the config parameters is passed to the `customLogger` to allow consistent filtering of the logs. ([#1110](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1110)).
- Functions for working with `BigNum` are now in the public `Contract.Numeric.BigNum` API ([#1109](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1109)).
- `PublicKey` and `Ed25519Signature` types now wrap `RawBytes` instead of `Bech32String`.
- `TypeLevel.Nat`, needed to implement `HasPlutusSchema`, gets exported in `Contract.PlutusData` ([#1143](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1143)).
- `MintingPolicy` to an enum consisting of `PlutusScript` or `NativeScript` ([#1069](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1069))
- `Contract.Scripts` `applyArgs` is now monomorphic on the script parameter ([#1069](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1069))
- Adapted Gero wallet extension to `preview` network in E2E test suite ([#1086](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1086))
- `Contact.TextEnvelope` how provides more type safe interface with simplified error handling ([#988](https://github.com/Plutonomicon/cardano-transaction-lib/issues/988))
- Forbid minting zero tokens. ([#1156](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1156))
- Modified functions `getWalletAddress`, `ownPubKeyHash`, `ownStakePubKeyHash`, `getWalletAddressWithNetworkTag` and `ownPaymentPubKeyHash` to return `Contract r (Array Adress)`. ([#1045](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1045))
- `pubKeyHashAddress` and `scriptHashAddress` now both accept an optional `Credential` that corresponds to the staking component of the address ([#1060](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1060))
- `utxosAt` and `getUtxo` now use Kupo internally, `utxosAt` returns `UtxoMap` without `Maybe` context. The users will need to set `kupoConfig` in `ConfigParams`. ([#1185](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1185))
- `Interval` type is redesigned to restrain some finite intervals to be expressed in the system ([#1041](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1041))

### Removed

- `balanceAndSignTxE`, `balanceAndSignTx`, `balanceAndSignTxs`, `balanceTxWithAddress`, `balanceTxsWithAddress`, `withBalancedAndSignedTx` and `withBalancedAndSignedTxs` from `Contract.Transaction` ([#1053](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1053))
- `ScriptOutput` is removed and therefore not exported by `Contract.Address` anymore. also, `Contract.Transaction` doesn't export `scriptOutputToTransactionOutput` anymore ([#652](https://github.com/Plutonomicon/cardano-transaction-lib/issues/652)).
- `Contract.TxConstraints.TxConstraint` type from public API. The users should rely on domain functions instead ([#1135](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1135))
- `Contract.Address.enterpriseAddressScriptHash`, `enterpriseAddressValidatorHash` - use `Contract.Address.addressPaymentValidatorHash` that works for base addresses as well, or `addressStakeValidatorHash` to get the stake component validator hash ([#1060](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1060))
- `Contract.Address.enterpriseAddressMintingPolicyHash` and `enterpriseAddressStakeValidatorHash` - these functions didn't make much sense (too specific, a very rare use case). ([#1060](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1060))
- `ScriptHash` type from the `Contract` interface (use `ValidatorHash`) ([#1060](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1060))
- `Contract.Address` re-exports from `Contract.Scripts` ([#1060](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1060))
- `Contract.Address.ownPubKeyHash` and `ownPubKeyHashes` - these are not needed, use `ownPaymentPubKeyHash` / `ownPaymentPubKeyHashes` ([#1211](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1211))
- `mustBalanceTxWithAddress` and `mustBalanceTxWithAddresses` balancer constraints - use a combination of `mustUseUtxosAtAddresses` and `mustSendChangeToAddress` to get the same behaviour ([#1243](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1243))

### Fixed

- Fix absence of `getUtxos` method in CIP-30 mock ([#1026](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1026))
- `awaitTxConfirmedWithTimeout` not respecting its timeout ([#1021](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1021))
- Absence of `serializeData` in Plutip ([#1078](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1078))
- Fix excessive reconnection attempts after `Contract` runtime finalization ([#965](https://github.com/Plutonomicon/cardano-transaction-lib/pull/965))
- Fix wallet extension error terminating the whole test suite ([#1209](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1209))
- Now we can process multiple time constraints ([#1124](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1124))
- E2E test suite didn't apply settings archive CLI option properly ([#1254](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1254))

### Runtime Dependencies

- [Ogmios](https://github.com/mlabs-haskell/ogmios) - v5.5.7
- [Kupo](https://github.com/CardanoSolutions/kupo) - v2.2.0
- [Cardano-Node](https://github.com/input-output-hk/cardano-node/) - v1.35.3
- [Ogmios-Datum-Cache](https://github.com/mlabs-haskell/ogmios-datum-cache) - commit 862c6bfcb6110b8fe816e26b3bba105dfb492b24
- [Plutip](https://github.com/mlabs-haskell/plutip) - commit 1c9dd05697d7cf55de8ca26f0756a75ed821bdfb

## [2.0.0] - 2022-09-12

### Added

- Plutip integration to run `Contract`s in local, private testnets ([#470](https://github.com/Plutonomicon/cardano-transaction-lib/pull/470))
- Ability to run `Contract`s in Plutip environment in parallel - `Contract.Test.Plutip.withPlutipContractEnv` ([#800](https://github.com/Plutonomicon/cardano-transaction-lib/issues/800))
- `withKeyWallet` utility that allows to simulate multiple actors in Plutip environment ([#663](https://github.com/Plutonomicon/cardano-transaction-lib/issues/663))
- `withStakeKey` utility that allows providing a stake key to be used by `KeyWallet`s in Plutip environment ([#838](https://github.com/Plutonomicon/cardano-transaction-lib/pull/838))
- `Alt` and `Plus` instances for `Contract`.
- `Contract.Utxos.getUtxo` call to get a single utxo at a given output reference
- `Contract.Monad.withContractEnv` function that constructs and finalizes a contract environment that is usable inside a bracket callback. **This is the intended way to run multiple contracts**. ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- `Contract.Monad.stopContractEnv` function to finalize a contract environment (close the `WebSockets`). It should be used together with `mkContractEnv`, and is not needed with `withContractEnv`. ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- `Contract.Config` module that contains everything needed to create and manipulate `ConfigParams`, as well as a number of `ConfigParams` fixtures for common use cases. ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- `Contract.Monad.askConfig` and `Contract.Monad.asksConfig` functions to access user-defined configurations. ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- `Contract.Config.WalletSpec` type that allows to define wallet parameters declaratively in `ConfigParams`, instead of initializing wallet and setting it to a `ContractConfig` ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- Faster initialization of `Contract` runtime due to parallelism. ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- `purescriptProject`'s `shell` parameter now accepts `packageLockOnly`, which if set to true will stop npm from generating `node_modules`. This is enabled for CTL developers
- `Contract.Transaction.awaitTxConfirmed` and `Contract.Transaction.awaitTxConfirmedWithTimeout`
- `Contract.TextEnvelope.textEnvelopeBytes` and family to decode the `TextEnvelope` format, a common format output by tools like `cardano-cli` to serialize values such as cryptographical keys and on-chain scripts
- `Contract.Wallet.isNamiAvailable` and `Contract.Wallet.isGeroAvailable` functions ([#558](https://github.com/Plutonomicon/cardano-transaction-lib/issues/558)])
- `Contract.Transaction.balanceTxWithOwnAddress` and `Contract.Transaction.balanceTxsWithOwnAddress` to override an `Address` used in `balanceTx` internally ([#775](https://github.com/Plutonomicon/cardano-transaction-lib/pull/775))
- `Contract.Transaction.awaitTxConfirmedWithTimeoutSlots` waits a specified number of slots for a transaction to succeed. ([#790](https://github.com/Plutonomicon/cardano-transaction-lib/pull/790))
- `Contract.Transaction.submitE` like submit but uses an `Either (Array Aeson) TransactionHash` to handle a SubmitFail response from Ogmios
- `Contract.Chain.waitNSlots`, `Contract.Chain.currentSlot` and `Contract.Chain.currentTime` a function to wait at least `N` number of slots and functions to get the current time in `Slot` or `POSIXTime`. ([#740](https://github.com/Plutonomicon/cardano-transaction-lib/issues/740))
- `Contract.Transaction.getTxByHash` to retrieve contents of an on-chain transaction.
- `project.launchSearchablePursDocs` to create an `apps` output for serving Pursuit documentation locally ([#816](https://github.com/Plutonomicon/cardano-transaction-lib/issues/816))
- `Contract.PlutusData.IsData` type class (`ToData` + `FromData`) ([#809](https://github.com/Plutonomicon/cardano-transaction-lib/pull/809))
- A check for port availability before Plutip runtime initialization attempt ([#837](https://github.com/Plutonomicon/cardano-transaction-lib/issues/837))
- `Contract.Address.addressToBech32` and `Contract.Address.addressWithNetworkTagToBech32` ([#846](https://github.com/Plutonomicon/cardano-transaction-lib/issues/846))
- `doc/e2e-testing.md` describes the process of E2E testing. ([#814](https://github.com/Plutonomicon/cardano-transaction-lib/pull/814))
- Added unzip to the `devShell`. New `purescriptProject.shell` flag `withChromium` also optionally adds Chromium to the `devShell` ([#799](https://github.com/Plutonomicon/cardano-transaction-lib/pull/799))
- Added paymentKey and stakeKey fields to the record in KeyWallet
- Added `formatPaymentKey` and `formatStakeKey` to `Wallet.KeyFile` and `Contract.Wallet` for formatting private keys
- Added `privatePaymentKeyToFile` and `privateStakeKeyToFile` to `Wallet.KeyFile` and `Contract.Wallet.KeyFile` for writing keys to files
- Added `bytesFromPrivateKey` to `Serialization`
- Improved error handling of transaction evaluation through Ogmios. This helps with debugging during balancing, as it requires the transaction to be evaluated to calculate fees. ([#832](https://github.com/Plutonomicon/cardano-transaction-lib/pull/832))
- `Contract.Hashing.transactionHash` to calculate the hash of the transaction ([#870](https://github.com/Plutonomicon/cardano-transaction-lib/pull/870))
- Flint wallet support ([#556](https://github.com/Plutonomicon/cardano-transaction-lib/issues/556))
- Support for `NativeScript`s in constraints interface: `mustPayToNativeScript` and `mustSpendNativeScriptOutput` functions ([#869](https://github.com/Plutonomicon/cardano-transaction-lib/pull/869))
- `Contract.Test.Cip30Mock` module to mock CIP-30 wallet interface using `KeyWallet`. The mock can be used for testing without a wallet (even in NodeJS environment). This increases test coverage for CTL code. ([#784](https://github.com/Plutonomicon/cardano-transaction-lib/issues/784))
- `Plutus.Types.AssocMap.AssocMap` now has `TraversableWithIndex`, `FoldableWithIndex`, `FunctorWithIndex` instances ([#943](https://github.com/Plutonomicon/cardano-transaction-lib/pull/943))
- The return value of `purescriptProject` now includes the project with its compiled `output` and its generated `node_modules` (under the `compiled` and `nodeModules` attributes, respectively) ([#956](https://github.com/Plutonomicon/cardano-transaction-lib/pull/956))
- `Contract.Utxos.getWalletUtxos` function that calls CIP-30 `getUtxos` method. ([#961](https://github.com/Plutonomicon/cardano-transaction-lib/issues/961))
- Lode wallet support ([#556](https://github.com/Plutonomicon/cardano-transaction-lib/issues/556))
- Added `Contract.Transaction.lookupTxHash` helper function ([#957](https://github.com/Plutonomicon/cardano-transaction-lib/issues/957))
- `Contract.Test.Utils` for making assertions about `Contract`s. ([#1005](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1005))
- `Examples.ContractTestUtils` demonstrating the use of `Contract.Test.Utils`. ([#1005](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1005))
- `mustNotBeValid` constraint which marks the transaction as invalid, allowing scripts to fail during balancing and for Ogmios to allow submission. ([#947](https://github.com/Plutonomicon/cardano-transaction-lib/pull/947))
- Constraints for creating outputs with reference scripts: `mustPayToScriptWithScriptRef`, `mustPayToPubKeyAddressWithDatumAndScriptRef`, `mustPayToPubKeyAddressWithScriptRef`, `mustPayToPubKeyWithDatumAndScriptRef`, `mustPayToPubKeyWithScriptRef` ([#946](https://github.com/Plutonomicon/cardano-transaction-lib/pull/946))
- Constraints for using reference validators and minting policies: `mustSpendScriptOutputUsingScriptRef`, `mustMintCurrencyUsingScriptRef`, `mustMintCurrencyWithRedeemerUsingScriptRef` ([#946](https://github.com/Plutonomicon/cardano-transaction-lib/pull/946))
- Constraint for attaching a reference input to a transaction: `mustReferenceOutput` ([#946](https://github.com/Plutonomicon/cardano-transaction-lib/pull/946))
- `DatumPresence` data type, which tags paying constraints that accept datum, to mark whether the datum should be inline or hashed in the transaction output. ([#931](https://github.com/Plutonomicon/cardano-transaction-lib/pull/931))
- Utility conversion functions `serializeData` and `deserializeData` between `PlutusData` and `CborBytes` to `Contract.PlutusData`. ([#1001](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1001))
- Added [CIP-30](https://cips.cardano.org/cip/CIP-0030) methods: `getNetworkId`, `getChangeAddress`, `getRewardAddresses`, `getUnusedAddresses`, `signData`, `isWalletAvailable`, `isEnabled`, `apiVersion`, `name` and `icon` to `Contract.Wallet` ([#974](https://github.com/Plutonomicon/cardano-transaction-lib/issues/974))

### Changed

- `PlutusScript` is now aware of which version of Plutus the script is for. The JSON representation has thus changed to reflect this and is not compatible with older JSON format.
- CTL's `overlay` no longer requires an explicitly passed `system`
- Switched to CSL for utxo min ada value calculation ([#715](https://github.com/Plutonomicon/cardano-transaction-lib/pull/715))
- Upgraded Haskell server to fully support Babbage-era transactions ([#733](https://github.com/Plutonomicon/cardano-transaction-lib/pull/733))
- Improved the collateral selection algorithm for `KeyWallet` ([#707](https://github.com/Plutonomicon/cardano-transaction-lib/pull/707))
- Switched to CSL for `PlutusScript` hashing ([#852](https://github.com/Plutonomicon/cardano-transaction-lib/pull/852))
- `runContract` now accepts `ConfigParams` instead of `ContractConfig` ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- `mkContractConfig` has been renamed to `mkContractEnv`. Users are advised to use `withContractEnv` instead to ensure proper finalization of WebSocket connections. ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- `ConfigParams` is now a type synonym instead of a newtype. `ContractConfig` has been renamed to `ContractEnv`.
- Moved logging functions to `Contract.Log` from `Contract.Monad` ([#727](https://github.com/Plutonomicon/cardano-transaction-lib/issues/727)
- Renamed `Contract.Wallet.mkKeyWalletFromPrivateKey` to `Contract.Wallet.mkKeyWalletFromPrivateKeys`.
- ServerConfig accepts a url `path` field ([#728](https://github.com/Plutonomicon/cardano-transaction-lib/issues/728)).
- Examples now wait for transactions to be confirmed and log success ([#739](https://github.com/Plutonomicon/cardano-transaction-lib/issues/739)).
- Updated CSL version to v11.0.0 ([#801](https://github.com/Plutonomicon/cardano-transaction-lib/issues/801))
- Better error message when attempting to initialize a wallet in NodeJS environment ([#778](https://github.com/Plutonomicon/cardano-transaction-lib/issues/778))
- The [`ctl-scaffold`](https://github.com/mlabs-haskell/ctl-scaffold) repository has been archived and deprecated and its contents moved to `templates.ctl-scaffold` in the CTL flake ([#760](https://github.com/Plutonomicon/cardano-transaction-lib/issues/760)).
- The CTL `overlay` output has been deprecated and replaced by `overlays.purescript`, `overlays.runtime`, and `overlays.ctl-server` ([#796](https://github.com/Plutonomicon/cardano-transaction-lib/issues/796) and [#872](https://github.com/Plutonomicon/cardano-transaction-lib/issues/872)).
- `buildCtlRuntime` and `launchCtlRuntime` now take an `extraServices` argument to add `docker-compose` services to the resulting Arion expression ([#769](https://github.com/Plutonomicon/cardano-transaction-lib/issues/769)).
- Use `cardano-serialization-lib` for fee calculation, instead of server-side code.
- `balanceAndSignTx` no longer silently drops error information via `Maybe`. The `Maybe` wrapper is currently maintained for API compatibility, but will be dropped in the future.
- Made it impossible to write unlawful `EncodeAeson` instances ([#490](https://github.com/Plutonomicon/cardano-transaction-lib/issues/490))
- The `ctl-server` component of the runtime is now optional and is only required when using the `applyArgs` endpoint ([#872](https://github.com/Plutonomicon/cardano-transaction-lib/issues/872)). Related changes include:
  - The `ctlServerConfig` fields of both `ConfigParams` and `PlutipConfig` now take a `Maybe ServerConfig`. In the case of `PlutipConfig`, a `Just` value will spawn the service inside the Plutip test. For the `ConfigParams` type, calls to `applyArgs` will fail when the field is set to `Nothing`.
  - The config accepted by `launchCtlRuntime` and `buildCtlRuntime` now takes a `ctl-server.enable` field. If `false`, `ctl-server` will not be launched.
- `SlotLength` and `RelativeTime` in `EraSummary` from Ogmios are now of type `Number` instead of `BigInt`. Also add `Maybe` around some functions in `Type.Interval` or changed it's signature to use `Number`. ([#868](https://github.com/Plutonomicon/cardano-transaction-lib/issues/868))
- The `ProtocolParameters` introduced in Alonzo (`prices`, `maxTxExUnits`, `maxBlockExUnits`, `maxValueSize`, `collateralPercent` and `maxCollateralInputs`) are no longer of type `Maybe` because we don't support pre-Alonzo eras. ([#971](https://github.com/Plutonomicon/cardano-transaction-lib/issues/971))
- Renamed `UtxoM` to `UtxoMap` ([#963](https://github.com/Plutonomicon/cardano-transaction-lib/pull/963))
- KeyWallet's `selectCollateral` field now allows multiple collateral to be selected, and is provided with `coinsPerUtxoByte` and `maxCollateralInputs` from the protocol parameters. ([#947](https://github.com/Plutonomicon/cardano-transaction-lib/pull/947))
- `mustPayWithDatumToPubKey`, `mustPayWithDatumToPubKeyAddress`, and `mustPayToScript` now expect a `DatumPresence` tag in their arguments to mark whether the datum should be inline or hashed in the transaction output. ((#931)[https://github.com/Plutonomicon/cardano-transaction-lib/pull/931))
- Switched to [blakejs](https://github.com/dcposch/blakejs) for blake2b hashing. `blake2b256Hash` and `blake2b256HashHex` functions are now pure ([#991](https://github.com/Plutonomicon/cardano-transaction-lib/pull/991))
- Updated ODC version, this includes a new function `getDatumsByHashesWithErrors`that does not discard errors, unlike `getDatumsByHashes`. **This update also changes the way we store transactions in the local database, meaning that we need to drop the `transactions` table.**

### Removed

- `Contract.Monad.traceTestnetContractConfig` - use `Contract.Config.testnetNamiConfig` instead (or other variants of `testnet...Config` for other wallets).
- `runContract_` - use `void <<< runContract`.
- `Contract.Aeson` module - use `Aeson` ([#938](https://github.com/Plutonomicon/cardano-transaction-lib/issues/938))

### Fixed

- Endless `awaitTxConfirmed` calls ([#804](https://github.com/Plutonomicon/cardano-transaction-lib/issues/804))
- Bug with collateral selection: only the first UTxO provided by wallet was included as collateral [(#723)](https://github.com/Plutonomicon/cardano-transaction-lib/issues/723)
- Bug with collateral selection for `KeyWallet` when signing multiple transactions ([#709](https://github.com/Plutonomicon/cardano-transaction-lib/pull/709))
- Bug when zero-valued non-Ada assets were added to the non-Ada change output ([#802](https://github.com/Plutonomicon/cardano-transaction-lib/pull/802))
- Error recovery logic for `SubmitTx` if the WebSocket connection is dropped ([#870](https://github.com/Plutonomicon/cardano-transaction-lib/pull/870))
- Properly implemented CIP-25 V2 metadata. Now there's no need to split arbitrary-length strings manually to fit them in 64 PlutusData bytes (CTL handles that). A new `Cip25String` type has been introduced (a smart constructor ensures that byte representation fits 64 bytes, as required by the spec). Additionally, a new `Metadata.Cip25.Common.Cip25TokenName` wrapper over `TokenName` is added to ensure proper encoding of `asset_name`s. There are still some minor differences from the spec:
  - We do not split strings in pieces when encoding to JSON
  - We require a `"version": 2` tag
  - `policy_id` must be 28 bytes
  - `asset_name` is up to 32 bytes. See https://github.com/cardano-foundation/CIPs/issues/303 for motivation
- `ogmios-datum-cache` now works on `x86_64-darwin`
- `TypedValidator` interface ([#808](https://github.com/Plutonomicon/cardano-transaction-lib/issues/808))
- `Contract.Address.getWalletCollateral` now works with `KeyWallet`.
- Removed unwanted error messages in case `WebSocket` listeners get cancelled ([#827](https://github.com/Plutonomicon/cardano-transaction-lib/issues/827))
- Bug in `CostModel` serialization - incorrect `Int` type ([#874](https://github.com/Plutonomicon/cardano-transaction-lib/issues/874))
- Use logger settings on Contract initialization ([#897](https://github.com/Plutonomicon/cardano-transaction-lib/issues/897))
- Disallow specifying less than 1 ADA in Plutip UTxO distribution ([#901](https://github.com/Plutonomicon/cardano-transaction-lib/pull/901))
- Bug in `TransactionMetadatum` deserialization ([#932](https://github.com/Plutonomicon/cardano-transaction-lib/issues/932))
- Fix excessive logging after the end of `Contract` execution ([#893](https://github.com/Plutonomicon/cardano-transaction-lib/issues/893))
- Add ability to suppress logs of successful `Contract` executions - with new `suppressLogs` config option the logs will be shown on error ([#768](https://github.com/Plutonomicon/cardano-transaction-lib/issues/768))
- Fix `runPlutipTest` not passing custom `buildInputs` ([#955](https://github.com/Plutonomicon/cardano-transaction-lib/pull/954))
- Problem parsing ogmios `SlotLength` and `RelativeTime` in era Summaries if those include non integer values. ([#906](https://github.com/Plutonomicon/cardano-transaction-lib/pull/906))
- Use `docs-search-0.0.12` that properly lists modules consisting only of re-exports ([#973](https://github.com/Plutonomicon/cardano-transaction-lib/issues/973))
- Inline datum in Ogmios transaction outputs are now parsed and preserved when converting to CTLs respective type. ([#931](https://github.com/Plutonomicon/cardano-transaction-lib/pull/931))

## [2.0.0-alpha] - 2022-07-05

This release adds support for running CTL contracts against Babbage-era nodes. **Note**: this release does not support Babbagge-era features and improvements, e.g. inline datums and reference inputs. Those feature will be implemented in `v2.0.0` proper.

### Added

- Support for using a `PrivateKey` as a `Wallet`.
- `mkKeyWalletFromFile` helper to use `cardano-cli`-style `skey`s
- Single `Plutus.Conversion` module exposing all `(Type <-> Plutus Type)` conversion functions ([#464](https://github.com/Plutonomicon/cardano-transaction-lib/pull/464))
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
- `d` and `extraEntropy` protocol parameters from protocol parameters update proposal

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
- `purescriptProject` (exposed via the CTL overlay) was reworked significantly. Please see the [updated example](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/ctl-as-dependency.md#using-ctls-overlay) in the documentation for more details.
- Switched to Ogmios for execution units evaluation ([#665](https://github.com/Plutonomicon/cardano-transaction-lib/pull/665))
- Changed `inputs` inside `TxBody` to be `Set TransactionInput` instead `Array TransactionInput`. This guarantees ordering of inputs inline with Cardano ([#641](https://github.com/Plutonomicon/cardano-transaction-lib/pull/661))
- Upgraded to Ogmios v5.5.0
- Change `inputs` inside `TxBody` to be `Set TransactionInput` instead `Array TransactionInput`. This guarantees ordering of inputs inline with Cardano ([#641](https://github.com/Plutonomicon/cardano-transaction-lib/pull/661)).

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
