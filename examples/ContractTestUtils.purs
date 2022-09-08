module Examples.ContractTestUtils
  ( ContractParams(ContractParams)
  , NonAdaAsset
  , contract
  ) where

import Contract.Prelude

import Contract.Address
  ( Address
  , PaymentPubKeyHash
  , StakePubKeyHash
  , getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , ownStakePubKeyHash
  , payPubKeyHashBaseAddress
  , payPubKeyHashEnterpriseAddress
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE, liftedM)
import Contract.PlutusData (Datum, PlutusData(Integer))
import Contract.Scripts (MintingPolicy)
import Contract.ScriptLookups as Lookups
import Contract.Test.Utils (ContractWrapAssertion, label)
import Contract.Test.Utils as TestUtils
import Contract.Transaction 
  ( awaitTxConfirmed
  , balanceAndSignTxE
  , getTxFinalFee
  , submit
  )
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName, Value)
import Contract.Value (lovelaceValueOf, singleton) as Value
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Examples.Helpers
  ( mustPayToPubKeyStakeAddress
  , mustPayWithDatumToPubKeyStakeAddress
  ) as Helpers

type NonAdaAsset = Tuple3 CurrencySymbol TokenName BigInt

newtype ContractParams = ContractParams
  { receiverPkh :: PaymentPubKeyHash
  , receiverSkh :: Maybe StakePubKeyHash
  , adaToSend :: BigInt
  , mintingPolicy :: MintingPolicy
  , tokensToMint :: NonAdaAsset
  }

derive instance Newtype ContractParams _

type ContractResult =
  { txFinalFee :: BigInt
  }

mkAssertions
  :: ContractParams
  -> Contract () (Array (ContractWrapAssertion () ContractResult))
mkAssertions params@(ContractParams p) = do
  senderAddress <-
    liftedM "Failed to get sender address" getWalletAddress
  receiverAddress <-
    liftedM "Failed to get receiver address" (getReceiverAddress params)
  pure
    [ TestUtils.assertGainAtAddress' (label receiverAddress "Receiver")
        p.adaToSend
    , TestUtils.assertLossAtAddress (label senderAddress "Sender")
        \{ txFinalFee } -> pure (p.adaToSend + txFinalFee)
    ]

contract :: ContractParams -> Contract () Unit
contract params@(ContractParams p) = do
  logInfo' "Running Examples.ContractTestUtils"
  ownPkh <- liftedM "Failed to get own PKH" ownPaymentPubKeyHash
  ownSkh <- ownStakePubKeyHash
  let
    adaValue :: Value
    adaValue = Value.lovelaceValueOf p.adaToSend

    nonAdaValue :: Value
    nonAdaValue = uncurry3 Value.singleton p.tokensToMint

    datum :: Datum
    datum = wrap (Integer $ BigInt.fromInt 42)

    constraints :: Constraints.TxConstraints Void Void
    constraints = mconcat
      [ Helpers.mustPayToPubKeyStakeAddress p.receiverPkh p.receiverSkh adaValue
      , Constraints.mustMintValue nonAdaValue
      , Helpers.mustPayWithDatumToPubKeyStakeAddress ownPkh ownSkh datum
          nonAdaValue
      ]

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy p.mintingPolicy

  assertions <- mkAssertions params 
  void $ TestUtils.withAssertions assertions do
    unbalancedTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
    balancedSignedTx <- liftedE $ balanceAndSignTxE unbalancedTx

    txId <- submit balancedSignedTx
    logInfo' $ "Tx ID: " <> show txId

    awaitTxConfirmed txId 
    logInfo' "Tx submitted successfully!"

    pure { txFinalFee: getTxFinalFee balancedSignedTx }

getReceiverAddress :: ContractParams -> Contract () (Maybe Address)
getReceiverAddress (ContractParams { receiverPkh, receiverSkh }) =
  getNetworkId <#> \networkId ->
    case receiverSkh of
      Just skh ->
        payPubKeyHashBaseAddress networkId receiverPkh skh
      Nothing ->
        payPubKeyHashEnterpriseAddress networkId receiverPkh
