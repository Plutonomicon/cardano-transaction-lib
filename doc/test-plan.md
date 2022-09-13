# CTL Test Plan

This document outlines CTL's test plan, i.e. a formalized description of testing CTL itself.

**Table of Contents**

- [User interactions](#user-interactions)
  - [Constraints/lookups](#constraintslookups)
    - [Exceptions](#exceptions)
  - [Other functionality](#other-functionality)
- [Acceptance criteria](#acceptance-criteria)
  - [Example contracts as tests](#example-contracts-as-tests)
  - [Test environments](#test-environments)
  - [Unit and integration testing](#unit-and-integration-testing)
    - [Required parsing tests](#required-parsing-tests)

## User interactions

This section outlines the parts of CTL's interface that we aim to guarantee function as expected. Each of the following functionality **must** be covered by an example contract (see our [Acceptance criteria](#acceptance-criteria) below for more details about coverage).

### Constraints/lookups

CTL's primary user interface is its constraints and lookups API, modeled after that of Plutus. We must ensure then that **all** of this interface is covered by complete examples (see [Acceptance criteria](#acceptance-criteria) below for a definition of an "example"). Each of the following constraints should be covered, along with the lookup that it implies (indicated in parentheses where applicable):

- [x] `mustMintValue` (`mintingPolicy`). Also implies
  - `mustMintCurrency`
- [x] `mustPayToScript` (`validator`)
- [x] `mustPayToPubKey`
  - **Note**: This invokes the same code as `mustPayToPubKeyAddress`, but does not include a stake key component
- [x] `mustPayToPubKeyAddress`
- [x] `mustMintValueWithRedeemer` (`mintingPolicy`). Also implies
  - `mustMintCurrencyWithRedeemer`
- [x] `mustSpendScriptOutput`
- [ ] `mustSpendPubKeyOutput`
  - **Note**: This constraint is included in our stake key integration with Plutip. We should still write a full contract that uses it
- [ ] `mustBeSignedBy`
  - **Note**: This constraint is also included in our Plutip stake key integration, as with `mustSpendPubKeyOutput`
- [ ] `mustHashDatum`
- [ ] `mustIncludeDatum`
- [ ] `mustPayWithDatumToPubKey`
- [ ] `mustPayWithDatumToPubKeyAddress`
- [ ] `mustProduceAtLeastTotal`. Also implies
  - [ ] `mustProduceAtLeast`
- [ ] `mustSatisfyAnyOf`
- [ ] `mustSpendAtLeastTotal`. Also implies
  - [ ] `mustSpendAtLeast`
- [ ] `mustSpendPubKeyOutput`
- [ ] `mustValidateIn`

The following constraints were added for `PlutusV2` features as part of our `v2.0.0` release. They do not have direct correspondances in `plutus-apps`:

- [x] `mustMintCurrencyUsingScriptRef`
- [x] `mustMintCurrencyWithRedeemerUsingScriptRef`
- [x] `mustPayToScriptWithScriptRef`
- [x] `mustPayToPubKeyAddressWithDatumAndScriptRef`
- [x] `mustPayToPubKeyAddressWithScriptRef`
- [x] `mustPayToPubKeyWithDatumAndScriptRef`
- [x] `mustPayToPubKeyWithScriptRef`
- [x] `mustSpendScriptOutputUsingScriptRef`

That release also included the following constraints for working with native scripts, which also have no `plutus-apps` analogue:

- [x] `mustPayToNativeScript`
- [x] `mustSpendNativeScriptOutput`

In addition, several redeemer combinations in a **single transaction** must be covered by tests or examples as well, namely

- [x] Two or more `Mint` redeemers
- [ ] Two or more `Spend` redeemers
- [ ] (At least) One each of a `Spend` and `Mint` redeemer

#### Exceptions

CTL does **not** currently support staking validators (see [#785](https://github.com/Plutonomicon/cardano-transaction-lib/issues/785)) so other `RedeemerTag`s are currently exempt from the above requirements. CTL also only makes guarantees for the **present era**, currently Babbage.

### Other functionality

In addition to the constraints/lookups listed above, there are several other critical pieces of functionality that CTL must guarantee. This functionality is subject to the same criteria as our constraints/lookups.

- `Contract.Transaction.*`
  - [x] `balanceTx`
  - [x] `signTransaction`
  - [x] `submit`
  - [x] `getTxByHash`
  - [x] `awaitTxConfirmed` (implies `awaitTxConfirmedWithTimeout`)
- `Contract.Scripts.*`
  - [x] `validatorHash`
  - [x] `mintingPolicy`
  - [ ] `applyArgs`
- `Contract.Hashing.*`
  - [x] `datumHash`
  - [x] `plutusScriptHash`
- `Contract.PlutusData.*`
  - [x] `getDatumByHash`
  - [x] `getDatumsByHashes`
- `Contract.Utxos.*`
  - [x] `utxosAt`

## Acceptance criteria

Coverage of particular functionality is measured by the inclusion of the **public** interface in full example contracts and tests. CTL's public API is defined in its `Contract.*` modules.

Most user interactions defined [above](#user-interactions) also call various parsers and serialization/deserialization code defined in CTL's private/internal modules. Acceptance criteria for these aspects of CTL are defined in [Unit testing](#unit-and-integration-testing) below.

### Example contracts as tests

In the case of CTL's constraints/lookups API, in order to be qualified as "covered", the relevant part of the API **must** be included in a **complete example** (such examples are currently contained in our [`examples/`](../examples) directory). Such examples must successfully

- build an unbalanced transaction specified using the constraints/lookups interface
  - **Note**: For implemented transaction features _not_ supported by our current constraints/lookups implementation, such as the features introduced by CIPs 31-33 (inline datums, etc...), modifying the transaction directly is also acceptable
- balance the transaction while calculating sufficient fees/execution units
- sign the transaction using the attached wallet (either a browser-based light wallet or our own `KeyWallet`)
- submit the transaction to the node via Ogmios

The functionality to achieve the above **must** be taken from our public API. That is, we must consume the public interface directly in all example contracts rather than importing internal CTL modules (anything outside of `Contract.*`).

#### Test environments

Furthermore, **all** example contracts must be able to execute in both of the environments that CTL supports. The following sections will refer to two different approachs for testing CTL:

- **Plutip** testing
  - This represents CTL's support for running in a Node.js environment (for `KeyWallet` examples), without a browser or light wallet
  - These examples must use our Plutip integrations (`Contract.Test.Plutip` and `purescriptProject.runPlutipTest` from our Nix infrastructure)
  - As these do not require network access, they can be executed in an entirely pure environment
  - These must be run on CI on each pull request against the CTL repository using `runPlutipTest` by adding the contract to `Test.Plutip`
- **e2e** (end-to-end) testing
  - This represents our browser integration and uses real light wallets and a public testnet
  - **All** currently supported wallets must be tested against each example contract
  - In the future, all e2e tests using real light wallets and a headless Chromium instance might also be run on CI
  - See [issue #929](https://github.com/Plutonomicon/cardano-transaction-lib/issues/929) for more details

Example contracts should be implemented such that the same contract can be reused in both environments. An example module might look like:

```purescript

module Examples.MintsToken
  ( example
  ) where

import Contract.Prelude

import Contract.Config (ConfigParams)
import Contract.Log (logInfo')
import Contract.Monad (launchAff_)

example :: ConfigParams () -> Effect Unit
example cfg = launchAff_ do
  runContract cfg do
    logInfo' "Running Examples.MintsToken"
    -- rest of the contract
```

Then, the exported `example` must be added to `Examples.ByUrl` to be run with `make e2e-test`. In some cases, it can be reused directly in a Plutip test (contained in the module `Test.Plutip`); in other cases, an indentical contract can be reimplemented inline in the Plutip test suite.

The **only** exception to the above rule is when the particulars of a contract preclude running it in one of the environments. For example, a contract that requires interaction between two or more wallets cannot currently be adapted to our e2e testing (as it assumes one connected wallet with one account).

### Unit and integration testing

CTL relies heavily on various runtime components to provide the necessary information to perform chain queries and construct transactions. We depend on a large amount of parsers, including serialization/deserialization code (i.e. to and from CTL's own domain type to `cardano-serialization-lib` FFI types), to consume the responses to websocket and HTTP requests made against CTL's runtime.

Although such parsers are included implicitly in the example contracts defined above, we must also ensure good coverage of edge cases that may arise when making runtime requests. Our approach to such testing can be formalized as:

- **Unit tests**
  - These tests rely on fixtures generated from runtime responses (see [`fixtures/`](../fixtures/test) for examples), stored in the same format as they are received in genuine responses
  - The corresponding tests can be largely pure functions which read the fixture and parse it
  - Success is defined as a parse returning a `Just` or `Right` value, depending on the parser
    - Due to the large number and semi-random nature of our test fixtures, we do not require comparing parsed values to an expected result
  - If possible, we should validate a parser against a component's _own_ test fixtures
    - See `Test.Ogmios.GenerateFixtures` for an example of this approach, which uses Ogmios' generated test vectors for our own testing
- **Integration tests**
  - These tests are run against a full runtime and make real requests to different components
  - These are intended to augment the unit tests described above and are a step below our full example contracts
  - These can either call effects from the `Contract` interface or its underlying `QueryM` monad stack

#### Required parsing tests

Currently, we require parsing tests for the following data structures, organized by dependency (including runtime dependencies):

- Ogmios
  - [x] `ChainTipQR`
  - [x] `UtxoQR`
  - [x] `CurrentEpoch`
  - [x] `SystemStart`
  - [x] `EraSummaries`
  - [x] `ProtocolParameters`
  - [x] `TxEvaluationR`
  - [x] `SubmitTxR`
- `ogmios-datum-cache`
  - [x] `GetDatumByHashR`
  - [ ] `GetDatumsByHashesR`
  - [ ] `GetTxByHashR`
- `cardano-serialization-lib`
  - `Transaction`
    - [x] Serialization
    - [x] Deserialization
  - `TxBody`
    - [x] Serialization
    - [x] Deserialization
  - `TransactionWitnessSet`
    - [x] Serialization
    - [x] Deserialization
