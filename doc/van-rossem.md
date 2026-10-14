# Van Rossem (PV11) compatibility notes

**Table of Contents**
<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Reference-input disjointness scoping](#reference-input-disjointness-scoping)
- [Related CTL API changes](#related-ctl-api-changes)
- [Runtime version bumps](#runtime-version-bumps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

The [Van Rossem intra-era hard
fork](https://intersectmbo.org/news/proposed-intra-era-hard-fork-to-protocol-version-11)
moves the Conway ledger to protocol version 11. Most of its changes are
transparent to CTL users (UPLC primitives, VRF-key uniqueness, cost-model
updates, and so on). Only one ledger-rule change surfaces in CTL's
transaction builder, described below. The rest of this document covers the
related CTL API changes and runtime version bumps shipped alongside PV11
compatibility in
[#1687](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1687).

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

## Related CTL API changes

Shipped in the same PR as PV11 compatibility:

- **`mkKeyWalletFromFiles` now accepts an optional DRep key file** as its
  third argument. Pass `Nothing` to preserve the previous behavior.
  ```purescript
  mkKeyWalletFromFiles
    :: FilePath          -- payment key
    -> Maybe FilePath    -- stake key
    -> Maybe FilePath    -- DRep key (new)
    -> Aff KeyWallet
  ```
- **New DRep key helpers** are exported from `Contract.Wallet.KeyFile`:
  `privateDrepKeyFromFile`, `privateDrepKeyFromTextEnvelope`,
  `privateDrepKeyToFile`. These parallel the existing payment and stake key
  functions.
- **`TestnetClusterConfig` changes.** The `era` field is removed as it
  became obsolete after upgrading to cardano-node 11.0.1. A new `pparams`
  field of type `BlockfrostProtocolParametersRaw` is added
  ([#1688](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1688)),
  letting you boot the local testnet with a custom set of protocol
  parameters.
- **`defaultTestnetProtocolParameters` now mirrors a Blockfrost mainnet
  snapshot** (fetched 2026-09-30), so the local testnet cluster boots at
  protocol version 11 with a consistent cost model
  ([#1692](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1692)).

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
