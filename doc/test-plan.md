# CTL Test Plan

This document outlines CTL's test plan, i.e. a formalized description of testing CTL itself.

## User interactions

This section outlines the parts of CTL's interface that we aim to guarantee function as expected. Each of the following functionality **must** be covered by a test or example (see our [Acceptance criteria](#acceptance-criteria) below for more details about coverage).

### Constraints/lookups

CTL's primary user interface is its constraints and lookups API, modeled after that of Plutus. We must ensure then that **all** of this interface is covered by complete examples (see [Acceptance criteria](#acceptance-criteria) below). Each of the following constraints should be covered, along with the lookup that it implies (indicated in parentheses where applicable):

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

In addition, several redeemer combinations in a **single transaction** must be covered by tests or examples as well, namely

- [x] Two or more `Mint` redeemers
- [ ] Two or more `Spend` redeemers
- [ ] (At least) One each of a `Spend` and `Mint` redeemer

#### Exceptions

CTL does **not** currently support staking validators (see [#785](https://github.com/Plutonomicon/cardano-transaction-lib/issues/785)) so other `RedeemerTag`s are currently exempt from the above requirements.

### Other functionality

- `Contract.Transaction.*`

  - [x] `balanceTx`
  - [x] `signTransaction`
  - [x] `submit`

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

Coverage of particular functionality is measured by the inclusion of the **public** interface in examples and tests.

In the case of CTL's constraints/lookups API, in order to be qualified as "covered", the relevant part of the API **must** be included in a **complete example** that builds, balances, and submits a transaction (such examples are currently contained in our [`examples/`](../examples) directory).

CTL only guarantees functionality for the present era, currently Babbage.
