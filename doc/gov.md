# Cardano governance in CTL

**Table of Contents**
<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [DReps](#dreps)
  - [Registering, updating and retiring a DRep](#registering-updating-and-retiring-a-drep)
  - [Script-based DReps](#script-based-dreps)
  - [Voting on governance proposals](#voting-on-governance-proposals)
- [ADA holders](#ada-holders)
  - [Delegating vote power to a DRep](#delegating-vote-power-to-a-drep)
  - [Creating a governance proposal](#creating-a-governance-proposal)
- [Governance-related Provider queries](#governance-related-provider-queries)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

Since the [Conway](https://cardano.org/glossary/conway-era/) hard fork, Cardano
has an on-chain governance system defined by
[CIP-1694](https://cips.cardano.org/cip/CIP-1694). Governance decisions, such as
changes to the protocol parameters, treasury withdrawals, hard-fork initiations
and constitution updates, are enacted via **governance actions** that are voted
on by three bodies: Delegated Representatives (DReps), the Constitutional
Committee (CC) and Stake Pool Operators (SPOs).

ADA holders participate indirectly by delegating their voting power to a DRep
(or to the pre-defined `AlwaysAbstain` / `AlwaysNoConfidence` options). Anyone
holding enough ADA to cover the required deposit can submit a governance
action.

CTL supports every on-chain governance operation available to end users:

- Registering, updating and retiring DReps (both pubkey-based and script-based)
- Voting on governance proposals as a DRep (pubkey or script)
- Delegating vote power from a stake credential to a DRep
- Submitting governance proposals, including protocol parameter updates

All the examples in this document live in [`examples/Gov/`](../examples/Gov/)
and use `submitTxFromBlueprint` from
[`Contract.Transaction`](../src/Contract/Transaction.purs) together with
[`TransactionBuilderStep`](https://github.com/mlabs-haskell/purescript-cardano-transaction-builder/blob/c8e835fc3952bcfe417389700ef97deee20d72f0/src/Cardano/Transaction/Builder.purs#L152-L160)
constructors. Code snippets below are condensed for readability. Refer to the
linked example files for the complete set of imports and helpers. If you have
not written a CTL contract before, start with
[getting-started.md](./getting-started.md).

**Note**: DRep-related wallet operations rely on
[CIP-95](https://cips.cardano.org/cip/CIP-95). Make sure your wallet is
connected with CIP-95 enabled:

```purescript
walletSpec =
  Just $ ConnectToGenericCip30 (walletName Eternl) { cip95: true }
```

## DReps

### Registering, updating and retiring a DRep

A pubkey-based DRep is identified by the DRep key of the connected wallet,
exposed via `Contract.Wallet.ownDrepPubKeyHash`. Registration requires locking
up a deposit of `drepDeposit` lovelace (a protocol parameter). Unregistration
returns the exact deposit that was paid, so you should always read it from the
chain rather than hardcode it. The optional `Anchor` carries an off-chain
metadata URL together with its hash.

The relevant certificates live in [`Cardano.Types.Certificate`](https://github.com/mlabs-haskell/purescript-cardano-types/blob/348fbbefa8bec5050e8492f5a9201ac5bb17c9d9/src/Cardano/Types/Certificate.purs#L49-L70):

- `RegDrepCert Credential Coin (Maybe Anchor)`, used to register
- `UpdateDrepCert Credential (Maybe Anchor)`, used to update metadata (and
  refresh the inactivity counter)
- `UnregDrepCert Credential Coin`, used to retire and reclaim the deposit

The example below (adapted from
[`examples/Gov/ManageDrep.purs`](../examples/Gov/ManageDrep.purs)) registers
the current wallet as a DRep, updates its metadata anchor, and retires it:

```purescript
import Cardano.Transaction.Builder (TransactionBuilderStep(IssueCertificate))
import Cardano.Types
  ( Certificate(RegDrepCert, UpdateDrepCert, UnregDrepCert)
  , Credential(PubKeyHashCredential)
  )
import Contract.Governance (queryRegisteredDrepInfo)
import Contract.ProtocolParameters (getProtocolParameters)
import Contract.Transaction
  ( awaitTxConfirmed
  , defaultBalancer
  , emptyBalancerCtx
  , submitTxFromBlueprint
  )
import Contract.Wallet (ownDrepPubKeyHash)

registerDrep :: Contract Unit
registerDrep = do
  drepCred <- PubKeyHashCredential <$> ownDrepPubKeyHash
  -- Read the current deposit from protocol parameters.
  drepDeposit <- _.drepDeposit <<< unwrap <$> getProtocolParameters
  { txHash } <- submitTxFromBlueprint
    { buildSteps:
        [ IssueCertificate
            (RegDrepCert drepCred drepDeposit Nothing)
            -- No credential witness: pubkey DReps are witnessed by signature.
            Nothing
        ]
    , balancer: defaultBalancer
    , balancerCtx: emptyBalancerCtx
    }
  awaitTxConfirmed txHash
```

To update the DRep's anchor, swap in `UpdateDrepCert drepCred (Just anchor)`.
To retire, use `UnregDrepCert drepCred depositToRefund`, where
`depositToRefund` **must equal the deposit that was originally paid**, not the
current protocol parameter. The right way to get it is via
`queryRegisteredDrepInfo` (see below), which returns a `DrepInfo` record
containing the original deposit.

### Script-based DReps

A DRep can also be a Plutus or native script. The script hash takes the place
of the pubkey hash in the credential, and because the ledger has to witness
the certificate, you must pass a `CredentialWitness` to `IssueCertificate`
instead of `Nothing`. The example in
[`examples/Gov/ManageDrepScript.purs`](../examples/Gov/ManageDrepScript.purs)
uses a trivial PlutusV3 minting policy as the DRep script:

```purescript
import Cardano.Transaction.Builder
  ( CredentialWitness(PlutusScriptCredential)
  , ScriptWitness(ScriptValue)
  , TransactionBuilderStep(IssueCertificate)
  )
import Cardano.Types (Credential(ScriptHashCredential))
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Cardano.Types.RedeemerDatum (unit) as RedeemerDatum

registerScriptDrep :: Contract Unit
registerScriptDrep = do
  drepScript <- alwaysMintsPolicyScriptV3 -- your DRep script
  let
    drepCred = ScriptHashCredential (PlutusScript.hash drepScript)
    drepCredWitness =
      PlutusScriptCredential (ScriptValue drepScript) RedeemerDatum.unit
  drepDeposit <- _.drepDeposit <<< unwrap <$> getProtocolParameters
  { txHash } <- submitTxFromBlueprint
    { buildSteps:
        [ IssueCertificate
            (RegDrepCert drepCred drepDeposit Nothing)
            (Just drepCredWitness)
        ]
    , balancer: defaultBalancer
    , balancerCtx: emptyBalancerCtx
    }
  awaitTxConfirmed txHash
```

The same `drepCredWitness` is passed when issuing `UpdateDrepCert` or
`UnregDrepCert` for the same script. The script will be executed against the
Conway script context and must approve the certificate.

### Voting on governance proposals

A registered DRep casts votes by issuing a `SubmitVotingProcedure` builder
step, which carries:

- the `Voter`. Use `Drep Credential` for a DRep (there is also `Cc Credential`
  for the Constitutional Committee and `Spo Ed25519KeyHash` for stake pool
  operators).
- a map `GovernanceActionId -> VotingProcedure`, so a single transaction can
  vote on several proposals at once.
- an optional `CredentialWitness`, used when the DRep is script-based
  (`Nothing` for a pubkey DRep).

A `VotingProcedure` wraps `Vote` (`VoteYes`, `VoteNo` or `VoteAbstain`) and an
optional `Anchor` with a rationale.

The following snippet, condensed from
[`examples/Gov/SubmitVote.purs`](../examples/Gov/SubmitVote.purs), votes `Yes`
on a proposal as a pubkey DRep:

```purescript
import Cardano.Transaction.Builder
  (TransactionBuilderStep(SubmitVotingProcedure))
import Cardano.Types
  ( Credential(PubKeyHashCredential)
  , GovernanceActionId
  , Vote(VoteYes)
  , Voter(Drep)
  , VotingProcedure(VotingProcedure)
  )
import Data.Map (singleton) as Map

submitVote :: GovernanceActionId -> Contract Unit
submitVote govActionId = do
  drepCred <- PubKeyHashCredential <$> ownDrepPubKeyHash
  { txHash } <- submitTxFromBlueprint
    { buildSteps:
        [ SubmitVotingProcedure (Drep drepCred)
            ( Map.singleton govActionId $
                VotingProcedure { vote: VoteYes, anchor: Nothing }
            )
            -- Script DRep? Pass (Just drepCredWitness) here.
            Nothing
        ]
    , balancer: defaultBalancer
    , balancerCtx: emptyBalancerCtx
    }
  awaitTxConfirmed txHash
```

For a script DRep, the only differences are the credential constructor
(`ScriptHashCredential`) and passing `Just drepCredWitness` as the third
argument of `SubmitVotingProcedure`. See
[`examples/Gov/SubmitVoteScript.purs`](../examples/Gov/SubmitVoteScript.purs).

Before voting, make sure the DRep is registered. Attempting to vote with an
unregistered credential will cause the transaction to be rejected. Query the
chain with `queryRegisteredDrepInfo` and, if it returns `Nothing`, prepend a
`RegDrepCert` to the build steps (both `SubmitVote.purs` examples show this
pattern).

## ADA holders

### Delegating vote power to a DRep

An ADA holder's voting power follows the stake credential. To delegate it you
issue one of two certificates:

- `VoteDelegCert StakeCredential DRep`, if the stake credential is already
  registered.
- `VoteRegDelegCert StakeCredential DRep Coin`, to register the stake
  credential and delegate in one certificate (requires paying
  `stakeAddressDeposit`).

`DRep` has three constructors: `DrepCred Credential` (delegate to a specific
DRep), `AlwaysAbstain` and `AlwaysNoConfidence`. The last two are the
predefined options that let a holder participate without picking a specific
DRep.

The following, taken from
[`examples/Gov/DelegateVoteAbstain.purs`](../examples/Gov/DelegateVoteAbstain.purs),
delegates to `AlwaysAbstain`:

```purescript
import Cardano.Transaction.Builder (TransactionBuilderStep(IssueCertificate))
import Cardano.Types.Certificate
  (Certificate(VoteDelegCert, VoteRegDelegCert))
import Cardano.Types.Credential (Credential(PubKeyHashCredential))
import Cardano.Types.DRep (DRep(AlwaysAbstain))
import Contract.Wallet
  (ownRegisteredPubStakeKeys, ownUnregisteredPubStakeKeys)

delegateAbstain :: Contract Unit
delegateAbstain = do
  -- Pick a stake key, and register it in the same tx if it isn't yet.
  unreg <- ownUnregisteredPubStakeKeys
  reg <- ownRegisteredPubStakeKeys
  { stakeKey, registered } <-
    case Array.head unreg of
      Just k -> pure { stakeKey: k, registered: false }
      Nothing -> case Array.head reg of
        Just k -> pure { stakeKey: k, registered: true }
        Nothing -> throwError $ error "no stake key"

  let stakeCred = wrap $ PubKeyHashCredential $ PublicKey.hash stakeKey
  stakeDeposit <- _.stakeAddressDeposit <<< unwrap <$> getProtocolParameters

  { txHash } <- submitTxFromBlueprint
    { buildSteps:
        [ IssueCertificate
            ( if registered
                then VoteDelegCert stakeCred AlwaysAbstain
                else VoteRegDelegCert stakeCred AlwaysAbstain stakeDeposit
            )
            Nothing
        ]
    , balancer: defaultBalancer
    , balancerCtx: emptyBalancerCtx
    }
  awaitTxConfirmed txHash
```

Replace `AlwaysAbstain` with `DrepCred someDrepCred` to delegate to a
particular DRep, or with `AlwaysNoConfidence` to vote `No` on every proposal
automatically.

### Creating a governance proposal

Any ADA holder can submit a governance proposal by posting a `SubmitProposal`
build step. The proposal carries a `GovernanceAction` payload, an anchor
describing the proposal off-chain, a deposit (read from `govActionDeposit`)
and a reward address that receives the deposit back if the proposal is
accepted or expires.

`GovernanceAction` has seven constructors (see [`Cardano.Types.GovernanceAction`](https://github.com/mlabs-haskell/purescript-cardano-types/blob/348fbbefa8bec5050e8492f5a9201ac5bb17c9d9/src/Cardano/Types/GovernanceAction.purs#L42-L49)):

| Constructor       | Meaning                                                 |
| ----------------- | ------------------------------------------------------- |
| `ChangePParams`   | Update protocol parameters                              |
| `TriggerHF`       | Initiate a hard fork                                    |
| `TreasuryWdrl`    | Withdraw funds from the treasury                        |
| `NoConfidence`    | Replace the Constitutional Committee with no-confidence |
| `NewCommittee`    | Add / remove CC members, change the quorum              |
| `NewConstitution` | Update the constitution and/or its guardrails script    |
| `Info`            | Informational proposal (no on-chain effect)             |

Here is the minimum version, submitting an `Info` proposal, distilled from
[`examples/Gov/SubmitVote.purs`](../examples/Gov/SubmitVote.purs):

```purescript
import Cardano.Transaction.Builder
  (TransactionBuilderStep(SubmitProposal))
import Cardano.Types
  ( Anchor(Anchor)
  , GovernanceAction(Info)
  , URL(URL)
  , VotingProposal(VotingProposal)
  )
import Contract.Wallet (getRewardAddresses)

myAnchor :: Anchor
myAnchor =
  Anchor
    { url: URL "https://example.com/my-proposal.json"
    , dataHash:
        unsafePartial $ fromJust $ decodeCbor $ wrap $ hexToByteArrayUnsafe
          "94b8cac47761c1140c57a48d56ab15d27a842abff041b3798b8618fa84641f5a"
    }

submitInfoProposal :: Contract GovernanceActionId
submitInfoProposal = do
  govActionDeposit <- _.govActionDeposit <<< unwrap <$> getProtocolParameters
  rewardAddr <- liftedM "Could not get reward address" $
    map (asRewardAddress <=< Array.head) getRewardAddresses
  { txHash } <- submitTxFromBlueprint
    { buildSteps:
        [ SubmitProposal
            ( VotingProposal
                { govAction: Info
                , anchor: myAnchor
                , deposit: unwrap govActionDeposit
                , returnAddr: rewardAddr
                }
            )
            -- Credential witness, only needed for actions guarded by scripts.
            Nothing
        ]
    , balancer: defaultBalancer
    , balancerCtx: emptyBalancerCtx
    }
  awaitTxConfirmed txHash
  pure $ wrap { transactionId: txHash, index: zero }
```

The transaction hash together with the proposal's index in the transaction's
`proposal_procedures` is the `GovernanceActionId` other parties will use to
vote on it. The index is `0` here because the transaction submits a single
proposal. If a transaction carries multiple proposals, each one gets a
sequential index (`0`, `1`, ...) in submission order.

Some actions require an extra credential witness. In particular,
`ChangePParams` and `TreasuryWdrl` are guarded by the current constitution's
**guardrails script**, so you must point `SubmitProposal`'s second argument
at a reference input holding that script. See
[`examples/Gov/UpdateProtocolParameters.purs`](../examples/Gov/UpdateProtocolParameters.purs)
for a complete protocol-parameter-update example, including how to obtain the
guardrails script hash and how to chain parameter updates via
`actionId: Just lastEnactedPParamsUpdateId`.

## Governance-related Provider queries

Three convenience queries live in
[`Contract.Governance`](../src/Contract/Governance.purs). They are thin
wrappers over the current `Provider`, so they transparently work on top of
Blockfrost or any other backend you plug in via
[custom query layers](./custom-query-layers.md):

- `queryRegisteredDrepInfo :: Credential -> Contract (Maybe DrepInfo)`.
  Returns `Nothing` if the credential is not a registered DRep. The resulting
  `DrepInfo` includes the **original deposit**, the expiry epoch and the
  anchor. Use it before issuing `UnregDrepCert` to pick the correct deposit,
  and before voting to decide whether to prepend a registration certificate.
- `queryProposalById :: GovernanceActionId -> Contract (Maybe Proposal)`.
  Fetches a proposal by its action ID. Useful to inspect the action payload,
  deposit, expiry and current status before casting a vote.
- `queryVotesOnProposal`. Given a `GovernanceActionId`, returns an
  `Array VoteOnProposal` listing all the votes already cast on a proposal (by
  DReps, SPOs and CC members).

Combined with the protocol parameters, these queries let you build governance
transactions without hardcoding any values that may drift between networks or
epochs:

```purescript
pparams <- unwrap <$> getProtocolParameters
let drepDeposit      = pparams.drepDeposit      -- for RegDrepCert
    govActionDeposit = pparams.govActionDeposit -- for SubmitProposal
    stakeDeposit     = pparams.stakeAddressDeposit -- for VoteRegDelegCert

drepInfo <- queryRegisteredDrepInfo drepCred
case drepInfo of
  Nothing   -> {- not registered: prepend RegDrepCert -}
  Just info -> {- already registered: use info.deposit for UnregDrepCert -}
```

In short: read deposits from `getProtocolParameters`, read on-chain state
(DReps and proposals) via `Contract.Governance`, and feed the results into the
appropriate `TransactionBuilderStep`. The runnable examples in
[`examples/Gov/`](../examples/Gov/) tie all of this together and are the
recommended reference when building your own governance workflows.
