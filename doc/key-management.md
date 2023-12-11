<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Key Management](#key-management)
  - [`KeyWallet` in CTL](#keywallet-in-ctl)
  - [Loading private keys as `KeyWallet`s](#loading-private-keys-as-keywallets)
    - [From `cardano-cli`-compatible JSON envelopes](#from-cardano-cli-compatible-json-envelopes)
    - [From CIP1852 mnemonics](#from-cip1852-mnemonics)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Key Management

The process of going from private key menmonic to a set of addresses that the wallet can use is well specified in [CIP-1852](https://cips.cardano.org/cip/CIP-1852).

For an address to be built, the user must provide some entropy for private key derivation and a *derivation path*, that specifies a list of cryptographic operations to perform on the "parent" or "root" key to get the "child" private key that can be used for signing.

The derivation path is a list of numbers, commonly written separated by `/` prefixed with `m`:

```
m
  / purpose' (always 1852)
  / coin_type' (always 1815)
  / accountIndex'
  / role (specified in CIP-1852)
  / addressIndex
```

`'` indicates *hardened* flag presence for the derivation function, which is a specific cryptographic feature that ensures that leaking the "child" key will not compromise the "parent".

`Role` is determined by the algorithm (different constants are used for payment, change and stake keys).

Effectively, `accountIndex` and `addressIndex` fully determine the address.

Note that within an account, the same stake key is used for all the addresses, thus making it trivial to track the movement of funds. This is done to simplify staking (one shared staking key controls all the funds on different payment addresses).

Most of the wallets are in "single address mode", which means that they will use only address index of 0. They may allow to use multiple accounts. Eternl is the only exception, that lets users create both multiple accounts and multiple addresses within an account.

## `KeyWallet` in CTL

CTL supports a special kind of wallet called `KeyWallet` - it wraps one or two private keys (stake key is optional) and can be used to sign transactions. `KeyWallet` is always in single-address mode, but it is possible to derive multiple `KeyWallet`s from menmonics using CTL.

## Loading private keys as `KeyWallet`s

### From `cardano-cli`-compatible JSON envelopes

[This guide](https://developers.cardano.org/docs/stake-pool-course/handbook/keys-addresses/) describes how to generate private keys using cardano-cli.

The signing key can be loaded to CTL using `WalletSpec`'s `UseKeys` constructor - either by providing a file or the private key itself. See [`examples/KeyWallet/Internal/Pkh2PkhContract.purs`](../examples/KeyWallet/Internal/Pkh2PkhContract.purs#L49).

### From CIP1852 mnemonics

A mnemonic is represented as a 12- or 24-word phare encoding private entropy from which the key is derived.

It can be converted to a `KeyWallet` given a derivation path and a config option that specifies whether to include a staking part of the address:

```purescript
type Cip1852DerivationPath =
  { accountIndex :: UInt
  , addressIndex :: UInt
  }

data StakeKeyPresence = WithStakeKey | WithoutStakeKey

Contract.Wallet.Key.mkKeyWalletFromMnemonic
  :: String -> Cip1852DerivationPath -> StakeKeyPresence -> Either String KeyWallet
```

`Contract.Wallet.withKeyWalletFromMnemonic` is another helper function that lets to construct and use wallets on the fly, which is convenient for [Plutip tests](./plutip-testing.md).

In `ContractParams`, these values can be conveniently passed as the `walletSpec` via the `UseMnemonic` constructor:

```purescript
data MnemonicSource
  = MnemonicString String
  | MnemonicFile FilePath

data WalletSpec
  = UseMnemonic MnemonicSource Cip1852DerivationPath StakeKeyPresence
  | ...
```
