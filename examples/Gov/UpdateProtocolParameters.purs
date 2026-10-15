module Ctl.Examples.Gov.UpdateProtocolParameters
  ( contract
  , example
  , main
  ) where

import Contract.Prelude

import Cardano.AsCbor (decodeCbor)
import Cardano.Transaction.Builder
  ( CredentialWitness(PlutusScriptCredential)
  , RefInputAction(ReferenceInput)
  , ScriptWitness(ScriptReference)
  , TransactionBuilderStep(IssueCertificate, SubmitProposal)
  )
import Cardano.Types
  ( Certificate(StakeRegistration)
  , Coin(Coin)
  , Credential(PubKeyHashCredential)
  , GovernanceAction(ChangePParams)
  , GovernanceActionId
  , ParameterChangeAction(ParameterChangeAction)
  , ScriptHash
  , TransactionInput
  , VotingProposal(VotingProposal)
  , emptyProtocolParamUpdate
  )
import Cardano.Types.BigNum (fromInt) as BigNum
import Cardano.Types.PublicKey (hash) as PublicKey
import Cardano.Types.RedeemerDatum (unit) as RedeemerDatum
import Contract.CborBytes (hexToCborBytes)
import Contract.Config
  ( ContractParams
  , KnownWallet(Eternl)
  , WalletSpec(ConnectToGenericCip30)
  , testnetConfig
  , walletName
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, liftedM, runContract)
import Contract.ProtocolParameters (getProtocolParameters)
import Contract.Transaction
  ( awaitTxConfirmed
  , defaultBalancer
  , submitTxFromBlueprint
  )
import Contract.Utxos (getUtxo)
import Contract.Wallet (getRewardAddresses, ownUnregisteredPubStakeKeys)
import Ctl.Examples.Gov.Internal.Common (asRewardAddress, dummyAnchor)
import Data.Array (head, singleton) as Array
import Data.Map (fromFoldable) as Map
import Data.Maybe (fromJust)
import Data.Newtype (modify, wrap)
import Partial.Unsafe (unsafePartial)

-- Three values below must be adjusted per network:
--   guardrailsScriptHash / guardrailsScriptOref
--     Hash of the current constitution's guardrails Plutus script and
--     the output reference of a UTxO carrying it as a reference script.
--     Both values change if the constitution is replaced via a
--     NewConstitution action. For mainnet/preview/preprod, see the
--     network's env page under book.world.dev.cardano.org (Guardrails
--     reference script UTxO section).
--
--   lastEnactedPParamsUpdateId
--     `GovernanceActionId` of the current enacted head of the
--     ParameterChange chain, which this proposal will extend. Pass
--     `Nothing` only on a fresh Conway bootstrap where no
--     ParameterChange action has ever been enacted.

guardrailsScriptHash :: ScriptHash
guardrailsScriptHash =
  unsafePartial fromJust
    $ decodeCbor
    =<< hexToCborBytes
      "fa24fb305126805cf2164c161d852a0e7330cf988f1fe558cf7d4a64"

guardrailsScriptOref :: TransactionInput
guardrailsScriptOref =
  wrap
    { transactionId:
        unsafePartial fromJust
          $ decodeCbor
          =<< hexToCborBytes
            "9aabbac24d1e39cb3e677981c84998a4210bae8d56b0f60908eedb9f59efffc8"
    , index: zero
    }

lastEnactedPParamsUpdateId :: GovernanceActionId
lastEnactedPParamsUpdateId =
  wrap
    { transactionId:
        unsafePartial fromJust
          $ decodeCbor
          =<< hexToCborBytes
            "78a9aafe2e4e14828efa8cd5202fec08c996a9a00c7d56b317b6a95a80510db3"
    , index: zero
    }

main :: Effect Unit
main = example $ testnetConfig
  { walletSpec =
      Just $ ConnectToGenericCip30 (walletName Eternl) { cip95: true }
  }

example :: ContractParams -> Effect Unit
example = launchAff_ <<< flip runContract contract

contract :: Contract Unit
contract = do
  logInfo' "Running Examples.Gov.UpdateProtocolParameters"
  pubStakeKey <- Array.head <$> ownUnregisteredPubStakeKeys
  let
    stakeCred =
      wrap <<< PubKeyHashCredential <<< PublicKey.hash <$>
        pubStakeKey
  govActionDeposit <- _.govActionDeposit <<< unwrap <$> getProtocolParameters
  rewardAddr <- liftedM "Could not get reward address" $
    map (asRewardAddress <=< Array.head)
      getRewardAddresses
  guardrailsScriptOut <-
    liftedM "Could not get output with guardrails script" $
      getUtxo guardrailsScriptOref
  let
    registerStakeAddress =
      maybe
        mempty
        (\x -> Array.singleton $ IssueCertificate (StakeRegistration x) Nothing)
        stakeCred
    govAction =
      ChangePParams $ ParameterChangeAction
        { pparamsUpdate:
            modify (_ { minfeeA = Just $ Coin $ BigNum.fromInt 30 })
              emptyProtocolParamUpdate
        , actionId: Just lastEnactedPParamsUpdateId
        , policyHash: Just guardrailsScriptHash
        }
  { txHash } <- submitTxFromBlueprint
    { buildSteps:
        registerStakeAddress <>
          [ SubmitProposal
              ( VotingProposal
                  { govAction
                  , anchor: dummyAnchor
                  , deposit: unwrap govActionDeposit
                  , returnAddr: rewardAddr
                  }
              )
              ( Just $
                  PlutusScriptCredential
                    (ScriptReference guardrailsScriptOref ReferenceInput)
                    RedeemerDatum.unit
              )
          ]
    , balancer: defaultBalancer
    , balancerCtx:
        { balancerConstraints: mempty
        , extraUtxos:
            Map.fromFoldable
              [ guardrailsScriptOref /\ guardrailsScriptOut
              ]
        }
    }
  awaitTxConfirmed txHash
  logInfo' "Tx submitted successfully!"
