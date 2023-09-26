# Plan for new constraints interface implementation

This document describes our plan for implementation of a better constraints interface as part of Catalyst's Fund10: https://cardano.ideascale.com/c/idea/101478

Here's a list of constraints with their new alternatives:

### `mustBeSignedBy`

```purescript
requireSignatureFrom :: PaymentPubKeyHash -> ?
```

### `mustDelegateStakeNativeScript`
### `mustDelegateStakePlutusScript`
### `mustDelegateStakePubKey`

```purescript
data DelegateStake
  = DelegatePubKeyHashStake StakePubKeyHash
  | DelegateNativeScriptStake NativeScriptStakeValidator
  | DelegatePlutusScriptStake PlutusScriptStakeValidator Redeemer

delegateStake :: (StakePubKeyHash | PlutusScript | NativeScript) -> PoolPubKeyHash
```

### `mustDeregisterStakeNativeScript`
### `mustDeregisterStakePlutusScript`
### `mustDeregisterStakePubKey`

```purescript
data DeregisterStake
  = DeregisterNativeScriptStake NativeScriptStakeValidator
  | DeregisterPlutusScriptStake PlutusScriptStakeValidator Redeemer
  | DeregisterPubKeyStake StakePubKeyHash

deregisterStake :: DeregisterStake -> ?
```

### `mustHashDatum` -> DELETE

Justification for deletion: it only differs from mustIncludeDatum by asserting that the provided hash matches. The user can do this comparison manually.

### `mustIncludeDatum`

```purescript
addDatum :: PlutusData -> ?
```

### `mustMintCurrency`
### `mustMintCurrencyUsingNativeScript`
### `mustMintCurrencyUsingScriptRef`
### `mustMintCurrencyWithRedeemer`
### `mustMintCurrencyWithRedeemerUsingScriptRef`

```purescript
mintWithPlutusScript
  :: (InputWithScriptRef | PlutusScript)
  -> Redeemer
  -> TokenName
  -> Amount
  -> ?
```

```purescript
mintWithNativeScript
  :: (InputWithScriptRef | NativeScript)
  -> TokenName
  -> Amount
  -> ?
```

### `mustMintValue` - DELETE

Justification for deletion: it is the same as `mustMintValueWithRedeemer` - but hardcodes `unitRedeemer`. The user should better specify the redeemer manually.

### `mustMintValueWithRedeemer` - DELETE

Justification for deletion: It is just a wrapper over `mustMintCurrencyWithRedeemer` that iterates over asset classes. The use case is too specific: when the user needs to mint a lot of tokens with the same redeemer. It's better to force the user to specify the same redeemer multiple times. And it does not matter whether we force the user using `fold` on constraint list manually, or let them construct a Value. It's better to give less ways to do one thing.

### `mustNotBeValid`

`markAsInvalid` - better name, it's literally what it does.

### `mustPayToNativeScript`
### `mustPayToNativeScriptAddress`
### `mustPayToPubKey`
### `mustPayToPubKeyAddress`
### `mustPayToPubKeyAddressWithDatum`
### `mustPayToPubKeyAddressWithDatumAndScriptRef`
### `mustPayToPubKeyAddressWithScriptRef`
### `mustPayToPubKeyWithDatum`
### `mustPayToPubKeyWithDatumAndScriptRef`
### `mustPayToPubKeyWithScriptRef`
### `mustPayToScript`
### `mustPayToScriptAddress`
### `mustPayToScriptAddressWithScriptRef`
### `mustPayToScriptWithScriptRef`

```purescript
payTo :: Address -> Maybe Datum -> Maybe ScriptRef -> ?
```

Some constraints thould be placed:

1. forbid paying to pkh with datum?
2. forbid paying to script without datum?
3. TBD

### `mustProduceAtLeast` -- DELETE

Justification for deletion:

TBD

### `mustReferenceOutput`

`referenceOutput :: TransactionInput -> ?`

### `mustRegisterPool`

`registerPool :: PoolRegistrationParams -> ?`

### `mustRegisterStakePubKey`
### `mustRegisterStakeScript`


```purescript
data StakeRegistration
  = StakePubKeyRegistration StakePubKeyHash
  | StakeScriptRegistration StakeScriptHash

registerStake :: StateRegistration -> ?
```

### `mustRetirePool`

```purescript
retirePool :: { pool :: PoolPubKeyHash, when :: Epoch } -> ?
```

### `mustSatisfyAnyOf` - DELETE

Justification: we don't really need clunky try/catch built in as constraint. Control structures should not be constraints.

### `mustSpendAtLeast` - DELETE

Justification for deletion:

TBD

### `mustSpendNativeScriptOutput`
### `mustSpendPubKeyOutput`
### `mustSpendScriptOutput`
### `mustSpendScriptOutputUsingScriptRef`

```purescript

data SpendOutputRef = TransactionInput /\ TransactionOutput /\ ScriptRef

data SpendOutput
  = PubKeyOutput
  | NativeScriptOutput NativeScript
  | PlutusScriptOutput Redeemer
  | NativeScriptWithRef SpendOutputRef
  | PlutusScriptWithRef SpendOutputRef Redeemer

spendOutput :: TransactionInput -> SpendOutput -> ?
```

### `mustValidateIn`

```purescript
validateIn :: (POSIXTimeInterval | (validityStartInterval /\ timeToLive)) -> ?
```

### `mustWithdrawStakeNativeScript`
### `mustWithdrawStakePlutusScript`
### `mustWithdrawStakePubKey`

```purescript
data WithdrawStake
  = WithdrawNativeScriptStake NativeScriptStakeValidator
  | WithdrawPlutusScriptStake PlutusScriptStakeValidator Redeemer
  | WithdrawPubKeyStake StakePubKeyHash

withdrawStake :: (StakePubKeyHash | NativeScript | PlutusScript)
```
