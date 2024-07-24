# Side by side comparison of CTL and Plutus off-chain contracts

We are going to compare two different off-chain contracts
both in Plutus and CTL.

For this discussion, we need to go through an overview of
both of them.

**Table of contents**
<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [About `Contract` in CTL and Plutus](#about-contract-in-ctl-and-plutus)
- [Contract code comparison](#contract-code-comparison)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## About `Contract` in CTL and Plutus

The current definition of `Contract` in CTL is :

```PureScript
type ContractEnv =
  { -- Internal type holding connections to backend services, ledger
    -- constants which are fixed during contract evaluation, and user defined
    -- values like the choice of wallet and logger.
  }

newtype Contract (a :: Type) = Contract (ReaderT ContractEnv Aff a)
```

The parameter `a`, as in `Plutus` also, refers to a return value wrapped by `Contract`.

Note that in Plutus right now we have the following definition for `Contract`:

```Haskell
type ContractEffs w e =
    '[ Error e
    ,  LogMsg Value
    ,  Writer w
    ,  Checkpoint
    ,  Resumable PABResp PABReq
    ]

type ContractEnv = (IterationID, RequestID)

newtype Contract w (s :: Row *) e a = Contract { unContract :: Eff (ContractEffs w e) a }
  deriving newtype (Functor, Applicative, Monad)
```

The Plutus `Contract` environment is specialized to just two values and is fixed.
Also, Plutus `Contract` uses a phantom type `s` for the contract schema
and parameters `w` for a writer and `e` for errors.
In the case of CTL we don't have the contract schema parameter or the writer
parameter since the definition of CTL allows performing arbitrary effects, from
the use of `Aff`. `Aff` allows us to use asynchronous effects, which has a
similar effect as using `IO` in Haskell, although isn't the same. While most
effectful actions are defined directly in terms of those provided by `Aff`,
logging is provided by a configurable logger stored in `ContractEnv`.


## Contract code comparison

Compare the contracts defined in week2 of the [Plutus pioneer program](https://plutus-pioneer-program.readthedocs.io/en/latest/pioneer/week2.html) with the [`AlwaysSucceeds`](../examples/AlwaysSucceeds.purs) example in CTL.
