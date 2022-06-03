# Side by side comparison of CTL and Plutus off-chain contracts

We are going to compare two different off-chain contracts 
both in Plutus and CTL. 

For this discussion, we need to go through an overview of 
both of them.


## About `Contract` in CTL and Plutus

The purpose of `Contract` in CTL is to bring the capabilities of 
`QueryMExtended` to the public API. 

The current definition of `Contract` in CTL is :

```PureScript
type QueryConfig (r :: Row Type) =
  { some_internal_parameters 
  | r
  }

type DefaultQueryConfig = QueryConfig ()

type QueryM (a :: Type) = ReaderT DefaultQueryConfig (LoggerT Aff) a

type QueryMExtended (r :: Row Type) (a :: Type) =
  ReaderT 
    (QueryConfig r)
    (LoggerT Aff)
    a

newtype Contract (r :: Row Type) (a :: Type) = Contract (QueryMExtended r a)
```


In CTL we have a general environment type 

```PureScript
forall (r :: Row Type) . QueryConfig r`
```

it stores the needed parameters to connect to an `Ogmios` server, 
wallets and more things, this configuration uses the PureScript native 
[row polymorphism](https://en.wikipedia.org/wiki/Row_polymorphism) to make it  extensible for both CTL developers and users.
You can find a little discussion about row polymorphism [here](https://hgiasac.github.io/posts/2018-11-18-Record-Row-Type-and-Row-Polymorphism.html).

The parameter `a` as in `Plutus` refers to a return value wrapped by `Contract`.


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
Also, Plutus `Contract` uses a phantom type `s` to contract schema 
and parameters `w` for a writer and `e` for errors. 
In the case of CTL we don't have the contract schema parameter or the writer 
parameter since CTL definition allows performing arbitrary effects. 
This is possible  since the definition of `LoggerT` is: 

```PureScript
newtype LoggerT m a = LoggerT ((Message -> m Unit) -> m a)
```

The use of `Aff` inside `LoggerT` allows us to use asynchronous effects 
inside the logger. In particular, this has a similar effect as using `IO` 
in Haskell, although isn't the same. Of course we can log to console 
using just `Aff` but `LoggerT` provide us with structured logging.



## Contract comparison

We can now begin to compare contracts. 

The most famous contracts are those contained as part of 
the [Plutus pioneer program](https://plutus-pioneer-program.readthedocs.io/en/latest/pioneer/week2.html) in week2. 
Both of them use the same on-chain contract that allows an arbitrary 
datum and arbitrary redeemer. 

### Signature and submit 

We are going to need the auxiliary PureScript function that isn't part 
of CTL: 

```PureScript
module ExampleModule where

import Contract.Prelude
import Contract.Monad (Contract, liftedE, liftedM, logInfo')
import Contract.ScriptLookups (ScriptLookups, mkUnbalancedTx)
import Contract.PlutusData (PlutusData)
import Contract.Transaction
  ( BalancedSignedTransaction(BalancedSignedTransaction)
  , balanceAndSignTx
  , submit
  )
import Types.TxConstraints (TxConstraints)
import Types.Transaction (TransactionHash)

buildBalanceSignAndSubmitTx
  :: ScriptLookups PlutusData
  -> TxConstraints Unit Unit
  -> Contract () TransactionHash
buildBalanceSignAndSubmitTx lookups constraints = do
  ubTx <- liftedE $ mkUnbalancedTx lookups constraints
  BalancedSignedTransaction bsTx <-
    liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
  txId <- submit bsTx.signedTxCbor
  logInfo' $ "Tx ID: " <> show txId
  pure txId
```


This function takes our lookups and constraints, then it: 

- Constructs an unbalanced transaction. 
- Attempts to balance the transaction.
- Request the user to sign this transaction.
- Submits the transaction.
- Returns the Id for the submitted transaction.

Note that some errors could happen in this process and 
the use of the functions `liftedE` and `liftedM` to 
propagate those errors and internalize them in the `Contract` monad. 
Also, as when we work with Haskell `IO` values, some other 
errors could happen performing the operations inside 
and `Aff` effect (some server could be unreachable, 
the user could fail provide the signature, etc).

This is a separate function as is almost boilerplate once we 
have set our environment.

We can see this function as a more explicit version of the 
process made by Plutus with the `submitTx` function: 

```Haskell 
-- | Build a transaction that satisfies the constraints, then submit it to 
-- | the network. The constraints do not refer to any 
-- | typed script inputs or outputs.
submitTx 
  :: forall w s e. AsContractError e 
    => TxConstraints Void Void -> Contract w s e CardanoTx
```

A major difference in both functions is the fact that we 
are also signing the transaction as part of the balance of 
the transaction.


### MustPayTo functions


In the case of Plutus `Contract`, we use the function 
`mustPayToOtherScript`, according to Plutus ledger, is defined as:

```Haskell
{-# INLINABLE mustPayToOtherScript #-}
-- | @mustPayToOtherScript vh d v@ locks the value @v@ with the given script
-- hash @vh@ alonside a datum @d@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint creates a script
-- output with @vh@, @d@ and @v@ and adds @d@ in the transaction's datum
-- witness set.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that @d@ is
-- part of the datum witness set and that the script transaction output with
-- @vh@, @d@ and @v@ is part of the transaction's outputs.
mustPayToOtherScript 
  :: forall i o. ValidatorHash -> Datum -> Value -> TxConstraints i o
```

While in the case of CTL we would use `mustPayToScript`: 

```PureScript
-- | Note that CTL does not have explicit equivalents of Plutus'
-- | `mustPayToTheScript` or `mustPayToOtherScript`, as we have no notion
-- | of a "current" script. Thus, we have the single constraint
-- | `mustPayToScript`, and all scripts must be explicitly provided to build
-- | the transaction.
mustPayToScript
  :: forall (i :: Type) (o :: Type)
   . ValidatorHash
  -> Datum
  -> Value
  -> TxConstraints i o
```

### The `give` contract 

Now we can write and compare the `give` contract. 
This contract takes and amount of Ada from our wallet 
an lock it to the script that validates any
transactions.

```Haskell
-- Haskell 
give :: 
  forall (w :: Type) (s :: Type) (e :: Type). 
    AsContractError e => 
      Integer -> Contract w s e ()
give amount = do
   let tx = mustPayToOtherScript vHash (Datum $ Constr 0 []) 
              $ Ada.lovelaceValueOf amount
   ledgerTx <- submitTx tx
   void $ awaitTxConfirmed $ txId ledgerTx
   logInfo @String $ printf "made a gift of %d lovelace" amount
```

We include some of the imports for the PureScript 
contract. 

```PureScript
-- PureScript
import Contract.PlutusData (PlutusData, unitDatum)
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints as Constraints
import Contract.Prelude
import Data.BigInt as BigInt

give :: ValidatorHash -> Contract () TransactionHash
give vhash = do
  let
    constraints :: Constraints.TxConstraints Unit Unit
    constraints = Constraints.mustPayToScript vhash unitDatum
      $ Value.lovelaceValueOf
      $ BigInt.fromInt 2_000_000

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty

  buildBalanceSignAndSubmitTx lookups constraints
```


### The `grab` contract

The Plutus `grab` example takes all the UTxOs locked by 
the on-chain contract that always validates a transaction, and spends 
them to get all in the wallet of the user running the example.
This isn't a problem as the example is intended to run inside a 
Plutus `EmulatorTrace` in a local toy environment.

```Haskell
-- Haskell
grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
   utxos <- utxoAt scrAddress
   let orefs = fst <$> Map.toList utxos
      lookups = Constraints.unspentOutputs utxos <>
                 Constraints.otherScript validator
      tx :: TxConstraints Void Void
      tx = 
        mconcat [mustSpendScriptOutput oref $ Redeemer $ I 17 | oref <- orefs]
   ledgerTx <- submitTxConstraintsWith @Void lookups tx
   void $ awaitTxConfirmed $ txId ledgerTx
   logInfo @String $ "collected gifts"
```

To talk about the grab contract in CTL we need to talk about some 
functions and types of CTL first. 

```PureScript
module Plutus.Types.Transaction ... 
.
.
.
type Utxo = Map TransactionInput TransactionOutput
newtype UtxoM = UtxoM Utxo
```

```PureScript
module Contract.Utxos ...
.
.
.
-- | This module defines query functionality via Ogmios to get utxos.
-- | Gets utxos at an (internal) `Address` in terms of a Plutus Address`.
-- | Results may vary depending on `Wallet` type. See `QueryM` for more details
-- | on wallet variance.
utxosAt :: forall (r :: Row Type). Address -> Contract r (Maybe UtxoM)
```


In the case of the CTL version of `grab`, we cannot use all the UTxOs locked by 
the validator that always validates, since the example is 
intended to run in the `testnet` and other people could have some 
values locked by the script.
This is the reason we assume we have already run the `give` contract to 
pay some `testAda` to the validator first, and then We got a `TransactionHash`.
We would use the `TransactionHash` to locate the right UTxO to spend. 

```PureScript
-- PureScript
grab
  :: ValidatorHash
  -> Validator
  -> TransactionHash
  -> Contract () Unit
grab vhash validator txId = do
  let scriptAddress = scriptHashAddress vhash
  UtxoM utxos <- fromMaybe (UtxoM Map.empty) <$> utxosAt scriptAddress
  case fst <$> find hasTransactionId (Map.toUnfoldable utxos :: Array _) of
    Just txInput ->
      let
        lookups :: Lookups.ScriptLookups PlutusData
        lookups = Lookups.validator validator
          <> Lookups.unspentOutputs utxos

        constraints :: TxConstraints Unit Unit
        constraints =
          Constraints.mustSpendScriptOutput txInput unitRedeemer
      in
        void $ buildBalanceSignAndSubmitTx lookups constraints
    _ ->
      logInfo' $ "The id "
        <> show txId
        <> " does not have output locked at: "
        <> show scriptAddress
  where
  hasTransactionId :: TransactionInput /\ _ -> Boolean
  hasTransactionId (TransactionInput tx /\ _) =
    tx.transactionId == txId
```

Notice the explicit signature in:

```
  fst <$> find hasTransactionId (Map.toUnfoldable utxos :: Array (_ /\ _))
```

Since PureScript has JS as the backend, `Array` is the most used container 
(instead of `List` as in Haskell), so, we prefer the use of `Array` over `List`
whenever it's adequate.
A downside of this is the lack of pattern matching over arbitrary arrays. 


Both versions of the contract use the same kind of constraints. 
Both need to add the validator and the UTxOs to the `lookups` 
and both need the `SpendScriptOutput` constraint. 
In the case of Plutus, this is done by a special function 
that accept lookups, while in CTL this is done by the explicit 
construction of an unbalanced transaction.

