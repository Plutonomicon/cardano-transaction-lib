module Ctl.Examples.PlutusV2.ReferenceInputsAndScripts
  ( contract
  , example
  , main
  ) where

import Contract.Prelude

import Cardano.Transaction.Builder
  ( CredentialWitness(PlutusScriptCredential)
  , OutputWitness(PlutusScriptOutput)
  , RefInputAction(ReferenceInput)
  , ScriptWitness(ScriptReference)
  , TransactionBuilderStep(MintAsset, SpendOutput, Pay)
  )
import Cardano.Types
  ( Credential(ScriptHashCredential)
  , OutputDatum(OutputDatum)
  , ScriptHash
  , TransactionOutput(TransactionOutput)
  , TransactionUnspentOutput
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Int as Int
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.RedeemerDatum as RedeemerDatum
import Cardano.Types.Transaction as Transaction
import Contract.Address (mkAddress)
import Contract.Config
  ( ContractParams
  , KnownWallet(Nami)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftContractM, runContract)
import Contract.Scripts (PlutusScript)
import Contract.Transaction
  ( ScriptRef(PlutusScriptRef)
  , TransactionHash
  , TransactionOutput
  , awaitTxConfirmed
  , lookupTxHash
  , submitTxFromBuildPlan
  )
import Contract.Utxos (utxosAt)
import Contract.Value (TokenName, Value)
import Contract.Value as Value
import Ctl.Examples.Helpers (mkAssetName) as Helpers
import Ctl.Examples.PlutusV2.Scripts.AlwaysMints (alwaysMintsPolicyScriptV2)
import Ctl.Examples.PlutusV2.Scripts.AlwaysSucceeds (alwaysSucceedsScriptV2)
import Data.Array (find) as Array
import Data.Map (empty, toUnfoldable) as Map
import Effect.Exception (error)

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Nami) { cip95: false }
  }

example :: ContractParams -> Effect Unit
example cfg = launchAff_ do
  runContract cfg contract

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.PlutusV2.ReferenceInputsAndScripts"
  validator <- alwaysSucceedsScriptV2
  mintsScript <- alwaysMintsPolicyScriptV2
  tokenName <- Helpers.mkAssetName "TheToken"
  let
    vhash :: ScriptHash
    vhash = PlutusScript.hash validator

    validatorRef :: ScriptRef
    validatorRef = PlutusScriptRef validator

    mpRef :: ScriptRef
    mpRef = PlutusScriptRef mintsScript

  logInfo' "Attempt to lock value"
  txId <- payToAlwaysSucceedsAndCreateScriptRefOutput vhash validatorRef mpRef
  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully, Try to spend locked values"
  spendFromAlwaysSucceeds vhash txId validator mintsScript
    tokenName

payToAlwaysSucceedsAndCreateScriptRefOutput
  :: ScriptHash -> ScriptRef -> ScriptRef -> Contract TransactionHash
payToAlwaysSucceedsAndCreateScriptRefOutput vhash validatorRef mpRef = do
  scriptAddress <- mkAddress (wrap $ ScriptHashCredential vhash) Nothing
  let
    value :: Value
    value = Value.lovelaceValueOf (BigNum.fromInt 2_000_000)
  Transaction.hash <$> submitTxFromBuildPlan Map.empty mempty
    [ Pay $ TransactionOutput
        { address: scriptAddress
        , amount: value
        , datum: Just $ OutputDatum PlutusData.unit
        , scriptRef: Just validatorRef
        }
    , Pay $ TransactionOutput
        { address: scriptAddress
        , amount: value
        , datum: Just $ OutputDatum PlutusData.unit
        , scriptRef: Just mpRef
        }
    , Pay $ TransactionOutput
        { address: scriptAddress
        , amount: value
        , datum: Just $ OutputDatum PlutusData.unit
        , scriptRef: Nothing
        }
    ]

spendFromAlwaysSucceeds
  :: ScriptHash
  -> TransactionHash
  -> PlutusScript
  -> PlutusScript
  -> TokenName
  -> Contract Unit
spendFromAlwaysSucceeds vhash txId validator mp tokenName = do
  scriptAddress <- mkAddress (wrap $ ScriptHashCredential vhash) Nothing
  scriptAddressUtxos <- utxosAt scriptAddress
  utxo <-
    liftM
      ( error
          ( "The id "
              <> show txId
              <> " does not have output locked at: "
              <> show scriptAddress
          )
      )
      $ Array.find hasNoRefScript
      $ lookupTxHash txId scriptAddressUtxos

  refValidatorInput /\ _ <-
    liftContractM "Could not find unspent output containing ref validator"
      $ Array.find (hasRefPlutusScript validator)
      $ Map.toUnfoldable scriptAddressUtxos

  refMpInput /\ _ <-
    liftContractM "Could not find unspent output containing ref minting policy"
      $ Array.find (hasRefPlutusScript mp)
      $ Map.toUnfoldable scriptAddressUtxos

  let
    mph = PlutusScript.hash mp
  spendTx <- submitTxFromBuildPlan scriptAddressUtxos mempty
    [ SpendOutput
        utxo
        ( Just
            $ PlutusScriptOutput
                (ScriptReference refValidatorInput ReferenceInput)
                RedeemerDatum.unit
                Nothing
        )
    , MintAsset mph tokenName (Int.fromInt 1)
        $ PlutusScriptCredential (ScriptReference refMpInput ReferenceInput)
            RedeemerDatum.unit
    ]
  awaitTxConfirmed $ Transaction.hash spendTx
  logInfo' "Successfully spent locked values and minted tokens."
  where
  hasRefPlutusScript
    :: PlutusScript
    -> _ /\ TransactionOutput
    -> Boolean
  hasRefPlutusScript plutusScript (_ /\ txOutput) =
    (unwrap txOutput).scriptRef == Just (PlutusScriptRef plutusScript)

  hasNoRefScript :: TransactionUnspentOutput -> Boolean
  hasNoRefScript utxo = isNothing (unwrap (unwrap utxo).output).scriptRef
