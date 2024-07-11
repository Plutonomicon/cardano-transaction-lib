<!-- DOCTOC SKIP -->

# Staking constraints in CTL

[Staking](https://cardano.org/stake-pool-delegation/) is the process of delegation of Ada claimed by a stake key or a script to a staking pool. Staking is an important part of Cardano operation, because it incentivizes block validators to actually perform their work.

[The explainer from Plutonomicon](https://github.com/Plutonomicon/plutonomicon/blob/main/stake-scripts.md) shows how staking works from a more technical perspective.

CTL supports all operations with stake:

- Registration/Deregistration of stake credentials (pubkeys, plutus and native scripts)
- Registration/Retirement of a stake pool
- Delegation of ADA to a stake pool
- Receiving rewards
- Withdrawing rewards

[Our tests](../test/Testnet/Staking.purs) include examples for each of the supported cases.
