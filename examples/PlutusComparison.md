
We are going to compare two different off-chain contracts 
both in Plutus and CTL. 

For this discussion, we need to go through an overview of 
both of them.


The purpose of `Contract` in CTL is to bring the capabilities of 
`QueryMExtended` to the public API. 

The current definition of `Contract` in CTL is :

```PureScript
type QueryConfig (r :: Row Type) =
  { ogmiosWs :: OgmiosWebSocket
  , datumCacheWs :: DatumCacheWebSocket
  , serverConfig :: ServerConfig
  , wallet :: Maybe Wallet
  -- should probably be more tightly coupled with a wallet
  , usedTxOuts :: UsedTxOuts
  , networkId :: NetworkId
  , slotConfig :: SlotConfig
  , logLevel :: LogLevel
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


In CTL we have a general environment type `forall r . QueryConfig r`, it stores
the needed parameters to connect to an `Ogmios` server  and
uses the PureScript native row polymorphism to make it extensible for both 
CTL developers and users.

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
Also, Plutus `Contract` uses a phantom type to allow users 
to put more effects on top and provide two parameters `w` for a writer 
and `e` for errors.

The definition of `LoggerT` is : 

```PureScript
newtype LoggerT m a = LoggerT ((Message -> m Unit) -> m a)
```

As CTL has been written in PureScript and it's intended to be used inside a 
browser, it makes little sense to allow more general logging capabilities. 
This is the reason why `QueryMExtended` uses directly `LoggerT` in its 
definition instead of allowing an arbitrary user writer. 

The use of `Aff` inside `LoggerT` allows us to use asynchronous effects inside the logger. 
In particular, this has a similar effect as using `IO` in Haskell.


With that little discussion, we can now begin to see contracts. 

The most famous contracts are those contained as part of 
the (Plutus pioneer program)[https://plutus-pioneer-program.readthedocs.io/en/latest/pioneer/week2.html]
in week2. Both of them use the same on-chain contract that 
allows an arbitrary datum and arbitrary redeemer. 

We going to need the auxiliary PureScript function : 

```PureScript
buildBalanceSingnAndSubmitTx
  :: Lookups.ScriptLookups PlutusData
  -> TxConstraints Unit Unit
  -> Contract () TransactionHash
buildBalanceSingnAndSubmitTx lookups constraints = do
  ubTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
  BalancedSignedTransaction bsTx <-
    liftedM "Failed to balance/sign tx" $ balanceAndSignTx ubTx
  txId <- submit bsTx.signedTxCbor
  logInfo' $ "Tx ID: " <> show txId
  pure txId
```


This function takes our lookups and constraints, then constructs a 
 unbalanced transaction, after that it tries to balance it and 
to get a signature for this transaction, then it 
submits the transaction and if all got right, it returns the Id
for the transaction wrapped inside the `Contract`. 

This is a separate function as is almost boilerplate once we 
have set our environment.



In the case of Plutus `Contract`, we use the function 
`mustPayToOtherScript`, according to Plutus ledger, it :

```Haskell
-- | mustPayToOtherScript vh d v locks the value v with the given script 
-- | hash vh alonside a datum d.
-- | If used in OffChain, this constraint creates a script output with 
-- | vh, d and v, and adds d in the 
-- | transaction's datum witness set.
mustPayToOtherScript 
  :: forall i o. ValidatorHash -> Datum -> Value -> TxConstraints i oSource#
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

With that said, we can write our contracts as: 


```Haskell
-- Haskell 
give :: 
  forall (w::Type) (s::Type) (e::Type) . 
    AsContractError e => 
      Integer -> Contract w s e ()
give amount = do
   let tx = mustPayToOtherScript vHash (Datum $ Constr 0 []) 
              $ Ada.lovelaceValueOf amount
   ledgerTx <- submitTx tx
   void $ awaitTxConfirmed $ txId ledgerTx
   logInfo @String $ printf "made a gift of %d lovelace" amount
```




```PureScript
-- PureScript
import Contract.PlutusData (PlutusData, unitDatum)
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints as Constraints
import Contract.Prelude
import Data.BigInt as BigInt

give :: Contract () TransactionHash
give =
  let
    constraints :: Constraints.TxConstraints Unit Unit
    constraints = Constraints.mustPayToScript vhash unitDatum
      $ Value.lovelaceValueOf
      $ BigInt.fromInt 2_000_000

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.validator validator
  in
  buildBalanceSingnAndSubmitTx lookups constraints
```


Notice how with the current API offered by CTL we need to provide the
validator as part of the `lookups` by ourselves. 


The Plutus `grab` example takes all the UTxOs locked by 
the on-chain contract that always validate a transaction, and spend 
them to get all in the wallet of the user running the example.
This isn't a problem as the example is intended to run inside a 
Plutus `EmulatorTrace` in a local toy environment.

```Haskell
-- Haskell
grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
   utxos <- utxoAt scrAddress
   let orefs   = fst <$> Map.toList utxos
      lookups  = Constraints.unspentOutputs utxos      <>
                 Constraints.otherScript validator
      tx :: TxConstraints Void Void
      tx       = mconcat [mustSpendScriptOutput oref $ Redeemer $ I 17 | oref <- orefs]
   ledgerTx <- submitTxConstraintsWith @Void lookups tx
   void $ awaitTxConfirmed $ txId ledgerTx
   logInfo @String $ "collected gifts"
```

To talk about the grab contract in CTL we need to talk a bout some 
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
-- | This module defines query functionality via Ogmios to get utxos. Gets utxos at an (internal) Address in terms of a Plutus Address. Results may vary depending on Wallettype. SeeQueryM` for more details on wallet variance.
utxosAt :: forall (r :: Row Type). Address -> Contract r (Maybe UtxoM)
```


In the case of CTL `grab`, we cannot use all the UTxOs locked by 
the validator that always validate, since the example is 
intended to run in the `testnet` and other people could lock some values. 
This is the reason we assume we have already runing the `give` contract to 
pay some `testAda` to the validator first and get a `TransactionHash`.
We would use the `TransactionHash` to locate the right UTxO to spend. 

```PureScript
-- PureScript
grab
  :: ValidatorHash
  -> Validator
  -> TransactionHash
  -> Contract () Unit
grab vhash validator txId = do
  let
    -- Casting a `ValidatorHash` to a plutus style Address
    scriptAddress = scriptHashAddress vhash
  UtxoM utxos <- 
    fromMaybe (UtxoM Map.empty) <$> utxosAt scriptAddress

  let
    filteredById =
      Map.filterWithKey hasTransactionId utxos

  case fst <$> (head <<< Map.toUnfoldable) filteredById of
    Just txInput ->
      let
        lookups :: Lookups.ScriptLookups PlutusData
        lookups = Lookups.validator validator
          <> Lookups.unspentOutputs utxos

        constraints :: TxConstraints Unit Unit
        constraints =
          Constraints.mustSpendScriptOutput txInput unitRedeemer
      in
        void
          $ buildSingnAndSubmitTx lookups constraints
    _ ->
      logInfo' $ "The id "
        <> show txId
        <> " does not have output locked at : "
        <> show scriptAddress
  where
  hasTransactionId :: forall t. TransactionInput -> t -> Boolean
  hasTransactionId (TransactionInput tx) _ =
    tx.transactionId == txId
```
