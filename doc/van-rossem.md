# van Rossem (PV11) compatibility notes

**Project:** cardano-transaction-lib (CTL)
**Release:** v10.0.1
**Author:** MLabs
**Report date:** 2026-10-28
**Consent:** MLabs consents to Intersect publishing this report in full.

**Table of Contents**
<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Changes shipped in M1](#changes-shipped-in-m1)
- [Changes shipped in M2](#changes-shipped-in-m2)
  - [Balancer and builder](#balancer-and-builder)
  - [Governance](#governance)
  - [Testnet defaults](#testnet-defaults)
  - [Documentation](#documentation)
- [Reference-input disjointness scoping](#reference-input-disjointness-scoping)
- [Testing summary](#testing-summary)
- [Runtime version bumps](#runtime-version-bumps)
- [Key Takeaways](#key-takeaways)
- [Known limitations and deferred work](#known-limitations-and-deferred-work)
- [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

The [van Rossem intra-era hard
fork](https://intersectmbo.org/news/cardano-upgrade-van-rossem-hard-fork)
moves the Conway ledger to protocol version 11. The hard fork was ratified
on-chain and is covered in Intersect's
[special-edition update](https://intersectmbo.org/news/intersect-update-special-edition).
Most of its changes are transparent to CTL (Cardano Transaction Library)
users (UPLC primitives, VRF-key uniqueness, cost-model updates, and so
on). Only one ledger-rule change surfaces in CTL's transaction builder,
described below. The rest of this document covers the changes shipped
across M1 and M2, a testing summary, and the runtime version bumps
required to run CTL at the van Rossem release.

The compatibility work was delivered across two milestones under the
Intersect Open Source Committee (OSC) grant. Milestone 1 ("Basic
Compatibility") shipped the runtime dependency bumps and the initial
PV11 node support. Milestone 2 ("Full van Rossem Support and Release
Readiness") covered the balancer and builder fixes, the governance
surface, documentation, and the tagged v10.0.1 release.

## Changes shipped in M1

- **cardano-node bumped to 11.0.1.** First node release with complete
  PV11 intra-era hard fork support.
  ([CTL#1687](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1687))
- **`TestnetClusterConfig.era` field removed.** After the cardano-testnet
  update that comes with the new node, the era field was no longer
  consulted and would mislead users if left in the public API.
  ([CTL#1687](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1687))
- **`mkKeyWalletFromFiles` now accepts an optional DRep key file**, plus
  new helpers `privateDrepKeyFromFile`, `privateDrepKeyFromTextEnvelope`,
  and `privateDrepKeyToFile`. The constructor previously only loaded
  payment and stake keys, which blocked signing governance actions from
  file-based wallets.
  ([CTL#1687](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1687))
- **Blockfrost Provider: PV11 cost model handling.** The van Rossem
  upgrade revised cost model parameters for every active Plutus language
  version, and Blockfrost's response schema changed accordingly. The
  provider now parses the updated format for V1, V2, and V3.
  ([blockfrost-provider#6](https://github.com/mlabs-haskell/purescript-cardano-blockfrost-provider/pull/6))
- **`pparams` field on `TestnetClusterConfig`.** Protocol parameters are
  now passed explicitly to cardano-testnet instead of relying on its
  implicit defaults, which removes a hidden dependency on the node's shipped
  defaults and makes test environments reproducible across node revisions.
  `Contract.Test.Testnet` additionally exposes
  `defaultTestnetClusterConfig`, `defaultTestnetProtocolParameters`, and
  `testnetConfigWithMaxExUnits` (a ready-made configuration for scripts
  that exceed the default execution budget).
  ([CTL#1688](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1688))
- **Runtime services bumped** to the minimum versions required for PV11.
  Kupo v2.11.0 → v2.12.0, Ogmios v6.14.0 → v7.0.0, Blockfrost Backend RYO
  v6.4.0 → v6.7.0.
  ([CTL#1689](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1689))

## Changes shipped in M2

### Balancer and builder

- **Balancer: input / reference-input overlap now allowed for non-V3
  transactions.** PV11 narrowed the disjointness check to the PlutusV3
  `TxInfo` translation, so the overlap is only invalid when a V3 script
  actually executes. The balancer previously rejected the overlap for all
  transactions and so refused inputs the ledger would accept. The guard
  was tightened to match the new rule.
  ([balancer#5](https://github.com/mlabs-haskell/purescript-cardano-transaction-balancer/pull/5),
  [CTL#1692](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1692))
- **Balancer: Plutus V1 detection now looks at reference scripts too.**
  Previously the balancer only inspected the witness set when deciding
  whether a transaction was V1, so a V1 script attached as a reference
  script was missed and led to wrong UTxO selection for V1 flows.
  Detection now covers both locations.
  ([balancer#5](https://github.com/mlabs-haskell/purescript-cardano-transaction-balancer/pull/5),
  [CTL#1692](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1692))
- **Balancer: `stakePoolDeposit` on `PoolRegistration` certificates.**
  The deposit was previously ignored during balancing, so the balanced
  transaction did not provide enough value and the ledger rejected it
  with a value-not-conserved error. The balancer now accounts for the
  deposit. `BalancerProtocolParameters` gains a required
  `stakePoolDeposit :: Coin` field.
  ([balancer#7](https://github.com/mlabs-haskell/purescript-cardano-transaction-balancer/pull/7),
  [CTL#1692](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1692))
- **Builder: spending datum-less UTxOs is now permitted.** CIP-69 lets V3
  scripts spend outputs with no datum, so the builder-level guard that
  rejected datum-less inputs was dropped. V1/V2 misuse of the same shape
  is still caught, but by the ledger at phase 1. The now-unreachable
  `WrongSpendWitnessType` constructor of `TxBuildError` was removed.
  ([builder#10](https://github.com/mlabs-haskell/purescript-cardano-transaction-builder/pull/10),
  [CTL#1692](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1692))
- **Balancer: `setScriptDataHash` guard matches the ledger rule.** The
  balancer could previously emit a script integrity hash for a
  transaction that carried an unused Plutus script in the witness set
  but had no redeemers, causing spurious `PPViewHashesDontMatch` or
  `ScriptIntegrityHashMismatch` failures at submission. The guard now
  mirrors the ledger (`null redeemers && null datums`).
  ([balancer#5](https://github.com/mlabs-haskell/purescript-cardano-transaction-balancer/pull/5),
  [CTL#1692](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1692))

### Governance

- **New `Contract.Governance` module.** Downstream applications previously
  had no provider-neutral way to inspect on-chain governance state. The
  new module exposes `queryProposalById`, `queryVotesOnProposal`, and
  `queryRegisteredDrepInfo` through the active `Provider`.
  ([CTL#1691](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1691))
- **`Cardano.Types.GovId` type with Bech32 converters.** Standardises the
  identifier used for proposals, DReps, and committee members, so
  consumers no longer need ad-hoc parsing. Also introduces an
  `emptyProtocolParamUpdate` helper for building protocol-parameter-update
  actions without hand-filling every field.
  ([types#22](https://github.com/mlabs-haskell/purescript-cardano-types/pull/22))
- **`Provider` governance queries added.** `getProposalById`,
  `getVotesOnProposal`, and `getRegisteredDrepInfo`, along with their
  result types (`Proposal`, `ProposalType`, `VoteOnProposal`, `DrepInfo`),
  are now part of the shared `Provider` interface so both backends can
  implement them against a single contract.
  ([provider#5](https://github.com/mlabs-haskell/purescript-cardano-provider/pull/5))
- **Blockfrost backend implementation** of the new governance queries,
  backed by the Blockfrost governance endpoints.
  ([blockfrost-provider#8](https://github.com/mlabs-haskell/purescript-cardano-blockfrost-provider/pull/8))
- **Kupmios backend implementation** of the new governance queries,
  backed by Ogmios, so Kupmios-based deployments reach parity with the
  Blockfrost backend.
  ([kupmios-provider#6](https://github.com/mlabs-haskell/purescript-cardano-kupmios-provider/pull/6))
- **Builder: committee certificate witnesses.** `useCertificateWitness`
  previously ignored `AuthCommitteeHotCert` and `ResignCommitteeColdCert`,
  so committee lifecycle transactions could not be signed through the
  builder. The witness is now attached to the committee cold credential.
  ([builder#9](https://github.com/mlabs-haskell/purescript-cardano-transaction-builder/pull/9),
  [CTL#1691](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1691))
- **`TestWalletSpec` reshape.** The previous `stakeKey :: Maybe
  PrivateStakeKey` and `drepKey :: Maybe PrivateDrepKey` fields forced
  each test to manage the raw key material itself. They were replaced
  with `withStakeKey :: Boolean` and `withDrepKey :: Boolean`, with the
  testnet harness generating the requested keys internally.
  ([CTL#1691](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1691))
- **Legacy `withStakeKey` helper and `InitialUTxOsWithStakeKey`
  deprecated** in favour of `TestWalletSpec` with `withStakeKey: true`.
  Uses of the old helper now emit a compile-time warning, giving
  downstream projects a migration window before the symbols are removed.
  ([CTL#1692](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1692))

### Testnet defaults

- **`defaultTestnetProtocolParameters` mirrors a Blockfrost mainnet
  snapshot** (fetched 2026-09-30). Previous defaults drifted from mainnet
  values and could mask behavioural differences between local tests and
  the real network. The pinned snapshot makes the local cluster boot at
  protocol version 11 with a consistent cost model.
  ([CTL#1692](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1692))

### Documentation

- **Two new documentation pages added:**
  [`doc/van-rossem.md`](./van-rossem.md) (this document) and
  [`doc/gov.md`](./gov.md), the end-to-end Cardano governance guide. Plus
  updates to the testnet-testing docs for the new `TestWalletSpec` API.
  Downstream projects now have a single page to migrate against for the
  van Rossem release and a separate guide for the newly exposed
  governance surface.
  ([CTL#1693](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1693))

## Reference-input disjointness scoping

Before PV11, the ledger enforced a *global* predicate requiring the reference
input set to be disjoint from the spending input set. PV11 removes that
predicate. The disjointness check now lives in the PlutusV3 `TxInfo`
translation, so it only fires when a V3 script actually executes.

Practical consequences:

- Non-Plutus and V1/V2 transactions may list the same UTxO as both a spending
  input and a reference input.
- V3 transactions still fail at phase 2 if any input also appears as a
  reference input.
- An unused V3 reference script does not fire the check. The check is
  triggered by a V3 *credential* being executed, not by V3 bytes being
  present in the transaction.

CTL's transaction builder does not expose a dedicated step for appending
bare reference inputs. Intentional overlap can be crafted via
`editTransaction` from `Cardano.Transaction.Edit` when needed.

The van Rossem test suite in
[`test/Testnet/Contract/VanRossem.purs`](../test/Testnet/Contract/VanRossem.purs)
exercises all four cases (no-Plutus overlap, V2 overlap, V3 overlap rejected,
unused V3 reference script) and is the canonical reference.

## Testing summary

All CTL test suites pass against the van Rossem dependency set at
cardano-node 11.0.1 (PV11).

| Suite                       | Entry point                 | Passed | Failed |
| --------------------------- | --------------------------- | ------ | ------ |
| Unit                        | `Test.Ctl.Unit`             | 246    | 0      |
| Local testnet integration   | `Test.Ctl.Testnet`          | 132    | 0      |
| Staking integration         | `Test.Ctl.Testnet.Staking`  | 4      | 0      |
| E2E browser                 | `Test.Ctl.E2E`              | 11     | 0      |

Each suite corresponds to a Nix check in `flake.nix` (`ctl-unit-test`,
`ctl-local-testnet-test`, `ctl-staking-test`, `ctl-e2e-test`). Counts are
taken from CI job
[6834](https://hercules-ci.com/github/Plutonomicon/cardano-transaction-lib/jobs/6834).

## Runtime version bumps

If you run CTL's backend yourself, update your runtime services. The
versions required at the van Rossem release:

| Component              | Minimum version |
| ---------------------- | --------------- |
| cardano-node           | 11.0.1          |
| ogmios                 | 7.0.0           |
| kupo                   | 2.12.0          |
| Blockfrost Backend RYO | 6.7.0           |

The project's Nix overlay pins all of the above, so if you consume CTL via
the overlay you only need to update your flake input. If you run the
services directly, bump them in your deployment config.

## Key Takeaways

- CTL v10.x bundles several breaking API changes alongside the PV11
  compatibility work. Downstream projects should review the CHANGELOG
  before upgrading.
- CTL's test coverage grew to include a dedicated PV11 regression suite
  alongside the pre-existing unit, integration, staking, and E2E
  suites. No regressions were observed.
- CTL's governance surface is now complete relative to CIP-1694 for the
  queries most downstream applications need. No further governance work
  is scheduled for this milestone.
- No deferred compatibility work remains. The v10.0.1 release is the
  tagged artefact against which downstream projects should pin.

## Known limitations and deferred work

No known behavioural limitations or deferred technical work remain for
the van Rossem release. One process item is carried forward: some of
the PRs delivered in this milestone were merged without a recorded
second-reviewer approval. Going forward, MLabs will require at least
one reviewer sign-off on every PR across CTL and its PureScript
dependency repos before merge.

## Summary

The v10.x line is the first CTL release series to support Protocol
Version 11, with v10.0.1 as the current release. The reference-input
disjointness relaxation is the only PV11-specific
behavioural change in the transaction builder, and the balancer, the
provider surface, and the test harness were revised alongside it. A new
`Contract.Governance` module and governance query implementations in
both the Blockfrost and Kupmios backends close the gaps identified in
CTL's CIP-1694 coverage. Two new documentation pages
(`doc/van-rossem.md` and `doc/gov.md`) and a dedicated PV11 regression
suite ship with the release. Downstream projects can migrate by pinning
CTL to v10.0.1 and bumping their runtime services to the versions
listed in the previous section.
