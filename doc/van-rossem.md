# Van Rossem (PV11) compatibility notes

**Table of Contents**
<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Reference-input disjointness scoping](#reference-input-disjointness-scoping)
- [Van Rossem-specific changes shipped in M1](#van-rossem-specific-changes-shipped-in-m1)
- [Van Rossem-specific changes shipped in M2](#van-rossem-specific-changes-shipped-in-m2)
- [Known limitations and deferred work](#known-limitations-and-deferred-work)
- [Testing summary](#testing-summary)
- [Runtime version bumps](#runtime-version-bumps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

The [Van Rossem intra-era hard
fork](https://intersectmbo.org/news/proposed-intra-era-hard-fork-to-protocol-version-11)
moves the Conway ledger to protocol version 11. Most of its changes are
transparent to CTL users (UPLC primitives, VRF-key uniqueness, cost-model
updates, and so on). Only one ledger-rule change surfaces in CTL's
transaction builder, described below. The rest of this document covers the
Van Rossem-specific changes shipped across M1 and M2, a testing summary, and
the runtime version bumps required to run CTL at the Van Rossem release.

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

CTL's transaction builder does not expose a dedicated step for appending bare
reference inputs. If you need to craft the overlap intentionally (uncommon),
reach for `editTransaction` from `Cardano.Transaction.Edit`:

```purescript
import Cardano.Transaction.Edit (editTransaction)
import Cardano.Types (Transaction, _body)
import Cardano.Types.TransactionBody (_referenceInputs)
import Data.Lens ((%~))

addReferenceInputs
  :: Array TransactionInput -> Transaction -> Transaction
addReferenceInputs refs =
  _body <<< _referenceInputs %~ (_ <> refs)
```

The Van Rossem test suite in
[`test/Testnet/Contract/VanRossem.purs`](../test/Testnet/Contract/VanRossem.purs)
exercises all four cases (no-Plutus overlap, V2 overlap, V3 overlap rejected,
unused V3 reference script) and is the canonical reference.

## Van Rossem-specific changes shipped in M1

Shipped in the same PR as PV11 compatibility
([#1687](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1687)):

- **`TestnetClusterConfig.era` field removed.** It became obsolete after
  upgrading to cardano-node 11.0.1 (the first node release with PV11
  intra-era hard fork support).

## Van Rossem-specific changes shipped in M2

- **Balancer allows input / reference-input overlap for non-V3
  transactions.** The balancer no longer treats the overlap as invalid
  for Plutus V1, V2, and non-Plutus transactions. V3 transactions still
  enforce disjointness at phase 2 as the ledger does.
  ([balancer#5](https://github.com/mlabs-haskell/purescript-cardano-transaction-balancer/pull/5))
- **Balancer Plutus V1 detection.** A transaction is now treated as V1
  when a V1 script is attached as a reference script, not only when it
  appears in the witness set. This can change which UTxOs the balancer
  picks when assembling V1 transactions.
  ([balancer#5](https://github.com/mlabs-haskell/purescript-cardano-transaction-balancer/pull/5))
- **`defaultTestnetProtocolParameters` now mirrors a Blockfrost mainnet
  snapshot** (fetched 2026-09-30), so the local testnet cluster boots at
  protocol version 11 with a consistent cost model
  ([#1692](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1692)).

## Known limitations and deferred work

None.

## Testing summary

All CTL test suites pass against the Van Rossem dependency set, tested against
cardano-node 11.0.1 (PV11).

| Suite                       | Entry point                 | Status |
| --------------------------- | --------------------------- | ------ |
| Unit                        | `Test.Ctl.Unit`             | pass   |
| Local testnet integration   | `Test.Ctl.Testnet`          | pass   |
| Staking integration         | `Test.Ctl.Testnet.Staking`  | pass   |
| E2E browser                 | `Test.Ctl.E2E`              | pass   |

Each suite corresponds to a Nix check in `flake.nix` (`ctl-unit-test`,
`ctl-local-testnet-test`, `ctl-staking-test`, `ctl-e2e-test`). No test
failures were encountered during the Van Rossem release validation.

## Runtime version bumps

If you run CTL's backend yourself, update your runtime services. The
versions required at the Van Rossem release:

| Component              | Minimum version |
| ---------------------- | --------------- |
| cardano-node           | 11.0.1          |
| ogmios                 | 7.0.0           |
| kupo                   | 2.12.0          |
| Blockfrost Backend RYO | 6.7.0           |

The project's Nix overlay pins all of the above, so if you consume CTL via
the overlay you only need to update your flake input. If you run the
services directly, bump them in your deployment config.
