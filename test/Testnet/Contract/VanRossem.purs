-- | Test suite for Van Rossem (PV11) hard-fork behavior and pre-existing
-- | Chang / CIP-69 / CIP-0110 rules that we treat as regression pins.
module Test.Ctl.Testnet.Contract.VanRossem
  ( suite
  ) where

import Prelude

import Cardano.Transaction.Builder
  ( DatumWitness(DatumValue)
  , OutputWitness(PlutusScriptOutput)
  , RefInputAction(ReferenceInput)
  , ScriptWitness(ScriptReference, ScriptValue)
  , TransactionBuilderStep(Pay, SpendOutput)
  )
import Cardano.Transaction.Edit (editTransaction)
import Cardano.Types
  ( Credential(ScriptHashCredential)
  , Language(PlutusV1)
  , OutputDatum(OutputDatum, OutputDatumHash)
  , PlutusScript(PlutusScript)
  , Transaction
  , TransactionInput
  , TransactionOutput(TransactionOutput)
  , TransactionUnspentOutput(TransactionUnspentOutput)
  , _body
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.DataHash (hashPlutusData)
import Cardano.Types.PlutusData (unit) as PlutusData
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.RedeemerDatum as RedeemerDatum
import Cardano.Types.TransactionBody (_referenceInputs)
import Cardano.Types.TransactionUnspentOutput (toUtxoMap)
import Contract.Address (mkAddress)
import Contract.BalanceTxConstraints (mustNotSpendUtxoWithOutRef)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.Test (ContractTest)
import Contract.Test.Testnet (InitialUTxOs, withWallets)
import Contract.Transaction
  ( ScriptRef(PlutusScriptRef)
  , TransactionHash
  , TxBlueprint
  , awaitTxConfirmed
  , buildTx
  , defaultBalancer
  , emptyBalancerCtx
  , lookupTxHash
  , signTransaction
  , submit
  , submitTxFromBlueprint
  )
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Contract.Wallet (getWalletAddresses, getWalletUtxos, withKeyWallet)
import Control.Monad.Error.Class (liftEither, try)
import Ctl.Examples.AlwaysSucceeds (alwaysSucceedsScript) as V1
import Ctl.Examples.PlutusV2.Scripts.AlwaysSucceeds (alwaysSucceedsScriptV2) as V2
import Ctl.Examples.PlutusV3.Scripts.AlwaysSucceeds (alwaysSucceedsScriptV3) as V3
import Data.Array as Array
import Data.Either (isLeft)
import Data.Lens ((%~))
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Tuple.Nested ((/\))
import Mote (group, test)
import Mote.TestPlanM (TestPlanM)
import Test.Spec.Assertions (shouldSatisfy)

-- Test cases:
--  1. V2 exec + datum-less V3-address output
--  2. V2 exec + datum-less V3-address reference input
--  3. Non-Plutus input/reference-input overlap
--  4. V2 exec + overlap
--  5. V3 exec + overlap - rejected
--  6. Unused V3 reference script does not fire disjointness
--  7. V3 spends a datum-less input
--  8. V1/V2 datum-less spend fails
--  9. V1 cannot spend a UTxO with a reference script
-- 10. V1 coexists with plain reference inputs
-- 11. V1 script invoked via reference script
-- 12. Paying to a V3 script address without a datum
-- 13. Paying to a V1/V2 script address without a datum

--------------------------------------------------------------------------------
-- Suite
--------------------------------------------------------------------------------

suite :: TestPlanM ContractTest Unit
suite = group "Van Rossem (PV11) + regression pins" do
  vanRossemSuite
  regressionPinSuite

--------------------------------------------------------------------------------
-- Van Rossem (PV11) - new behavior
--------------------------------------------------------------------------------

vanRossemSuite :: TestPlanM ContractTest Unit
vanRossemSuite = group "Van Rossem - new behavior" do

  test
    "1. V1/V2 script executes in a tx that contains a datum-less script output"
    do
      -- Previously rejected by the V1/V2 `validOutputs` predicate at
      -- phase-2. PV11 removes the rejection.
      --
      -- Shape:
      --  * Lock a UTxO under V2 always-succeeds with an inline datum,
      --    then spend it with the V2 script.
      --  * Additionally, produce an output at a V3 script address with
      --    `datum: Nothing` (only V3 addresses may legally hold
      --    datum-less UTxOs, per CIP-69). V3 does not execute.
      --  * Expect: confirmed.
      withWallets standardDistribution \alice -> withKeyWallet alice do
        v2Validator <- V2.alwaysSucceedsScriptV2
        v3Validator <- V3.alwaysSucceedsScriptV3
        v2Addr <- mkAddress
          (wrap $ ScriptHashCredential $ PlutusScript.hash v2Validator)
          Nothing
        v3Addr <- mkAddress
          (wrap $ ScriptHashCredential $ PlutusScript.hash v3Validator)
          Nothing

        logInfo' "Locking value at V2 script address with inline datum"
        { txHash: lockTxHash } <- submitTxFromBlueprint
          { buildSteps:
              [ Pay $ TransactionOutput
                  { address: v2Addr
                  , amount: Value.lovelaceValueOf $ BigNum.fromInt 3_000_000
                  , datum: Just $ OutputDatum PlutusData.unit
                  , scriptRef: Nothing
                  }
              ]
          , balancer: defaultBalancer
          , balancerCtx: emptyBalancerCtx
          }
        awaitTxConfirmed lockTxHash

        v2Utxos <- utxosAt v2Addr
        v2Utxo <- liftContractM "Could not find locked V2 UTxO"
          $ Array.head
          $ lookupTxHash lockTxHash v2Utxos

        logInfo'
          "Spending V2 UTxO and creating a datum-less output at V3 address"
        { txHash } <- submitTxFromBlueprint
          { buildSteps:
              [ SpendOutput v2Utxo
                  ( Just $ PlutusScriptOutput
                      (ScriptValue v2Validator)
                      RedeemerDatum.unit
                      Nothing
                  )
              , Pay $ TransactionOutput
                  { address: v3Addr
                  , amount: Value.lovelaceValueOf $ BigNum.fromInt 2_000_000
                  , datum: Nothing
                  , scriptRef: Nothing
                  }
              ]
          , balancer: defaultBalancer
          , balancerCtx:
              { balancerConstraints: mempty
              , extraUtxos: toUtxoMap [ v2Utxo ]
              }
          }
        awaitTxConfirmed txHash

  test
    "2. V2 script executes in a tx with a datum-less reference-input UTxO locked at script address"
    do
      -- Previously rejected by the V2 `validReferenceInput` predicate at
      -- phase-2. PV11 removes the rejection.
      --
      -- Shape:
      --  * Create an output at a V3 script address with `datum: Nothing`
      --    (only V3 addresses may legally hold datum-less UTxOs).
      --  * Lock another UTxO under V2 always-succeeds with an inline
      --    datum.
      --  * Spend the V2-locked UTxO and list the V3-address output as a
      --    reference input. V3 does not execute.
      --  * Expect: confirmed.
      withWallets standardDistribution \alice -> withKeyWallet alice do
        v2Validator <- V2.alwaysSucceedsScriptV2
        v3Validator <- V3.alwaysSucceedsScriptV3
        v2Addr <- mkAddress
          (wrap $ ScriptHashCredential $ PlutusScript.hash v2Validator)
          Nothing
        v3Addr <- mkAddress
          (wrap $ ScriptHashCredential $ PlutusScript.hash v3Validator)
          Nothing

        logInfo' "Creating a datum-less UTxO at V3 script address"
        { txHash: v3LockTxHash } <- submitTxFromBlueprint
          { buildSteps:
              [ Pay $ TransactionOutput
                  { address: v3Addr
                  , amount: Value.lovelaceValueOf $ BigNum.fromInt 2_000_000
                  , datum: Nothing
                  , scriptRef: Nothing
                  }
              ]
          , balancer: defaultBalancer
          , balancerCtx: emptyBalancerCtx
          }
        awaitTxConfirmed v3LockTxHash

        v3Utxos <- utxosAt v3Addr
        v3RefUtxo <- liftContractM "Could not find V3 UTxO to reference"
          $ Array.head
          $ lookupTxHash v3LockTxHash v3Utxos
        let v3RefOref = (unwrap v3RefUtxo).input

        logInfo' "Locking value at V2 script address with inline datum"
        { txHash: v2LockTxHash } <- submitTxFromBlueprint
          { buildSteps:
              [ Pay $ TransactionOutput
                  { address: v2Addr
                  , amount: Value.lovelaceValueOf $ BigNum.fromInt 3_000_000
                  , datum: Just $ OutputDatum PlutusData.unit
                  , scriptRef: Nothing
                  }
              ]
          , balancer: defaultBalancer
          , balancerCtx: emptyBalancerCtx
          }
        awaitTxConfirmed v2LockTxHash

        v2Utxos <- utxosAt v2Addr
        v2Utxo <- liftContractM "Could not find locked V2 UTxO"
          $ Array.head
          $ lookupTxHash v2LockTxHash v2Utxos

        logInfo' "Spending V2 UTxO with a datum-less reference-input UTxO"
        txHash <- submitTxWithEdit
          (addReferenceInputs [ v3RefOref ])
          { buildSteps:
              [ SpendOutput v2Utxo
                  ( Just $ PlutusScriptOutput
                      (ScriptValue v2Validator)
                      RedeemerDatum.unit
                      Nothing
                  )
              ]
          , balancer: defaultBalancer
          , balancerCtx:
              { balancerConstraints: mempty
              , extraUtxos: toUtxoMap [ v2Utxo, v3RefUtxo ]
              }
          }
        awaitTxConfirmed txHash

  test
    "3. Input/reference-input overlap accepted when no Plutus executes"
    do
      -- PV11 removes the global disjoint-reference-input predicate. A
      -- non-Plutus tx may list the same UTxO as both a spending input
      -- and a reference input.
      --
      -- Shape:
      --  * Pick a wallet UTxO `A`.
      --  * Build a tx that spends `A` and simultaneously lists `A` as a
      --    reference input. No script credentials, no witnesses beyond
      --    the vkey signature.
      --  * Expect: awaitTxConfirmed succeeds.
      withWallets standardDistribution \alice -> withKeyWallet alice do
        unspentA <- pickWalletUtxo
        let orefA = (unwrap unspentA).input
        txHash <- submitTxWithEdit
          (addReferenceInputs [ orefA ])
          { buildSteps: [ SpendOutput unspentA Nothing ]
          , balancer: defaultBalancer
          , balancerCtx: emptyBalancerCtx
          }
        awaitTxConfirmed txHash

  test
    "4. Input/reference-input overlap accepted when only V1/V2 executes"
    do
      -- Same shape as item 3, plus a required V2 script that must run to
      -- validate one of the inputs (or a mint policy). Overlap remains
      -- legal because the V3 TxInfo disjointness check does not fire.
      --
      -- Shape:
      --  * Lock a UTxO `S` under a V2 validator.
      --  * Pick an additional wallet UTxO `A`.
      --  * Build a tx: SpendOutput `S` with the V2 validator, SpendOutput
      --    `A`, list `A` as a reference input as well.
      --  * Expect: awaitTxConfirmed succeeds.
      withWallets standardDistribution \alice -> withKeyWallet alice do
        validator <- V2.alwaysSucceedsScriptV2
        scriptAddr <- mkAddress
          (wrap $ ScriptHashCredential $ PlutusScript.hash validator)
          Nothing

        logInfo' "Locking value at V2 script address"
        { txHash: lockTxHash } <- submitTxFromBlueprint
          { buildSteps:
              [ Pay $ TransactionOutput
                  { address: scriptAddr
                  , amount: Value.lovelaceValueOf $ BigNum.fromInt 3_000_000
                  , datum: Just $ OutputDatum PlutusData.unit
                  , scriptRef: Nothing
                  }
              ]
          , balancer: defaultBalancer
          , balancerCtx: emptyBalancerCtx
          }
        awaitTxConfirmed lockTxHash

        scriptUtxos <- utxosAt scriptAddr
        scriptUtxo <- liftContractM "Could not find locked V2 UTxO"
          $ Array.head
          $ lookupTxHash lockTxHash scriptUtxos
        unspentA <- pickWalletUtxo
        let orefA = (unwrap unspentA).input

        logInfo' "Spending V2 UTxO with input/reference-input overlap"
        txHash <- submitTxWithEdit
          (addReferenceInputs [ orefA ])
          { buildSteps:
              [ SpendOutput scriptUtxo
                  ( Just $ PlutusScriptOutput
                      (ScriptValue validator)
                      RedeemerDatum.unit
                      Nothing
                  )
              , SpendOutput unspentA Nothing
              ]
          , balancer: defaultBalancer
          , balancerCtx:
              { balancerConstraints: mempty
              , extraUtxos: toUtxoMap [ scriptUtxo ]
              }
          }
        awaitTxConfirmed txHash

  test
    "5. Input/reference-input overlap rejected when V3 executes"
    do
      -- V3's TxInfo translation still forbids overlap. This is the
      -- negative counterpart to item 4.
      --
      -- Shape:
      --  * Lock a UTxO `S` under a V3 validator.
      --  * Pick an additional wallet UTxO `A`.
      --  * Build a tx: SpendOutput `S` with the V3 validator, SpendOutput
      --    `A`, list `A` also as a reference input.
      --  * Expect: submit fails (V3 phase-2 rejects during TxInfo
      --    translation). Use `try` + `shouldSatisfy isLeft`.
      withWallets standardDistribution \alice -> withKeyWallet alice do
        validator <- V3.alwaysSucceedsScriptV3
        scriptAddr <- mkAddress
          (wrap $ ScriptHashCredential $ PlutusScript.hash validator)
          Nothing

        logInfo' "Locking value at V3 script address"
        { txHash: lockTxHash } <- submitTxFromBlueprint
          { buildSteps:
              [ Pay $ TransactionOutput
                  { address: scriptAddr
                  , amount: Value.lovelaceValueOf $ BigNum.fromInt 3_000_000
                  , datum: Just $ OutputDatum PlutusData.unit
                  , scriptRef: Nothing
                  }
              ]
          , balancer: defaultBalancer
          , balancerCtx: emptyBalancerCtx
          }
        awaitTxConfirmed lockTxHash

        scriptUtxos <- utxosAt scriptAddr
        scriptUtxo <- liftContractM "Could not find locked V3 UTxO"
          $ Array.head
          $ lookupTxHash lockTxHash scriptUtxos
        unspentA <- pickWalletUtxo
        let orefA = (unwrap unspentA).input

        result <- try do
          logInfo' "Attempting V3 spend with input/reference-input overlap"
          txHash <- submitTxWithEdit
            (addReferenceInputs [ orefA ])
            { buildSteps:
                [ SpendOutput scriptUtxo
                    ( Just $ PlutusScriptOutput
                        (ScriptValue validator)
                        RedeemerDatum.unit
                        Nothing
                    )
                , SpendOutput unspentA Nothing
                ]
            , balancer: defaultBalancer
            , balancerCtx:
                { balancerConstraints: mempty
                , extraUtxos: toUtxoMap [ scriptUtxo ]
                }
            }
          awaitTxConfirmed txHash
        result `shouldSatisfy` isLeft

  test
    "6. Unused V3 reference script does not fire disjointness"
    do
      -- Only V3 scripts required by a script credential count as executed.
      -- An unused V3 script must not fire the check.
      --
      -- The obvious variant - unused V3 attached to `witnessSet.plutusScripts` -
      -- is unreachable in practice: the ledger's UTXOW rule rejects extraneous
      -- witness scripts (`ExtraneousScripts`, ogmios code 3104). Reference
      -- scripts on reference-input UTxOs are allowed to sit around without
      -- being required, so that is the shape we exercise here.
      --
      -- Shape:
      --  * Deploy a UTxO carrying the V3 script as `referenceScript`.
      --  * Spend wallet UTxO A and also list A as a reference input.
      --  * List the ref-script UTxO as a reference input too. No credential
      --    invokes the V3 script.
      --  * Expect: confirmed.
      withWallets standardDistribution \alice -> withKeyWallet alice do
        v3Baggage <- V3.alwaysSucceedsScriptV3
        aliceAddr <- liftContractM "No wallet address"
          =<< (Array.head <$> getWalletAddresses)

        logInfo' "Deploying V3 script as a reference script"
        { txHash: refDeployTxHash } <- submitTxFromBlueprint
          { buildSteps:
              [ Pay $ TransactionOutput
                  { address: aliceAddr
                  , amount: Value.lovelaceValueOf $ BigNum.fromInt 5_000_000
                  , datum: Nothing
                  , scriptRef: Just $ PlutusScriptRef v3Baggage
                  }
              ]
          , balancer: defaultBalancer
          , balancerCtx: emptyBalancerCtx
          }
        awaitTxConfirmed refDeployTxHash

        aliceUtxos <- utxosAt aliceAddr
        refScriptUtxo <- liftContractM "Could not find ref-script UTxO"
          $ Array.head
          $ lookupTxHash refDeployTxHash aliceUtxos
        let refScriptOref = (unwrap refScriptUtxo).input

        -- Pick a wallet UTxO for A - must not be the ref-script UTxO itself.
        unspentA <- pickWalletUtxoExcluding refScriptOref
        let orefA = (unwrap unspentA).input

        logInfo'
          "Building non-V3 tx with overlap plus an unused V3 reference script"
        txHash <- submitTxWithEdit
          (addReferenceInputs [ orefA, refScriptOref ])
          { buildSteps: [ SpendOutput unspentA Nothing ]
          , balancer: defaultBalancer
          , balancerCtx:
              { balancerConstraints: mustNotSpendUtxoWithOutRef refScriptOref
              , extraUtxos: toUtxoMap [ unspentA, refScriptUtxo ]
              }
          }
        awaitTxConfirmed txHash

--------------------------------------------------------------------------------
-- Pre-existing regression pins (Chang / CIP-69 / CIP-0110)
--------------------------------------------------------------------------------

regressionPinSuite :: TestPlanM ContractTest Unit
regressionPinSuite = group "Regression pins" do

  test "7. V3 script can spend a datum-less input" do
    -- CIP-69 / Chang. A V3 script address may hold a UTxO without a
    -- datum, and the script can spend it.
    --
    -- Shape:
    --  * Pay to a V3 script address with `datum: Nothing`.
    --  * Spend that UTxO with the V3 validator (no DatumWitness).
    --  * Expect: awaitTxConfirmed succeeds.
    withWallets standardDistribution \alice -> withKeyWallet alice do
      validator <- V3.alwaysSucceedsScriptV3
      scriptAddr <- mkAddress
        (wrap $ ScriptHashCredential $ PlutusScript.hash validator)
        Nothing

      logInfo' "Locking value at V3 script address with datum: Nothing"
      { txHash: lockTxHash } <- submitTxFromBlueprint
        { buildSteps:
            [ Pay $ TransactionOutput
                { address: scriptAddr
                , amount: Value.lovelaceValueOf $ BigNum.fromInt 3_000_000
                , datum: Nothing
                , scriptRef: Nothing
                }
            ]
        , balancer: defaultBalancer
        , balancerCtx: emptyBalancerCtx
        }
      awaitTxConfirmed lockTxHash

      logInfo' "Spending the datum-less UTxO with V3 validator"
      scriptUtxos <- utxosAt scriptAddr
      utxo <- liftContractM "Could not find locked V3 UTxO"
        $ Array.head
        $ lookupTxHash lockTxHash scriptUtxos
      { txHash: spendTxHash } <- submitTxFromBlueprint
        { buildSteps:
            [ SpendOutput utxo
                ( Just $ PlutusScriptOutput
                    (ScriptValue validator)
                    RedeemerDatum.unit
                    Nothing
                )
            ]
        , balancer: defaultBalancer
        , balancerCtx:
            { balancerConstraints: mempty
            , extraUtxos: toUtxoMap [ utxo ]
            }
        }
      awaitTxConfirmed spendTxHash

  test "8. V1/V2 spending datum-less input fails at phase-1" do
    -- CIP-69. A V1 or V2 script cannot spend a UTxO without a datum.
    -- Ledger rejects at phase-1 with `MissingRequiredDatums`.
    --
    -- Shape:
    --  * Send a UTxO to a V2 script address with `datum: Nothing`. Note
    --    this creates dead value that the builder allows (item 13).
    --  * Try to spend that UTxO with the V2 script.
    --  * Expect: submit fails with a phase-1 error mentioning
    --    MissingRequiredDatums.
    withWallets standardDistribution \alice -> withKeyWallet alice do
      v1Validator <- V1.alwaysSucceedsScript
      v2Validator <- V2.alwaysSucceedsScriptV2
      assertDatumLessSpendFails v1Validator
      assertDatumLessSpendFails v2Validator

  test "9. V1 cannot spend a UTxO with a reference script attached" do
    -- CIP-0110 / Chang. A V1 script may not spend a UTxO whose output
    -- carries a `referenceScript`. Ledger rejects.
    --
    -- The inline-datum sub-case is already covered by
    -- Contract.purs:1470 "InlineDatum fails because PlutusV1 script is
    -- used". Only the reference-script sub-case is new coverage.
    --
    -- Shape:
    --  * Pay to a V1 script address with a datum hash and an attached
    --    reference script (any script will do as the ref).
    --  * Try to spend the UTxO with the V1 script.
    --  * Expect: submit fails.
    withWallets standardDistribution \alice -> withKeyWallet alice do
      v1Validator <- V1.alwaysSucceedsScript
      refScript <- V2.alwaysSucceedsScriptV2
      v1Addr <- mkAddress
        (wrap $ ScriptHashCredential $ PlutusScript.hash v1Validator)
        Nothing
      let
        datumUnit = PlutusData.unit
        dHash = hashPlutusData datumUnit

      logInfo'
        "Locking value at V1 address with hashed datum + attached ref script"
      { txHash: lockTxHash } <- submitTxFromBlueprint
        { buildSteps:
            [ Pay $ TransactionOutput
                { address: v1Addr
                , amount: Value.lovelaceValueOf $ BigNum.fromInt 3_000_000
                , datum: Just $ OutputDatumHash dHash
                , scriptRef: Just $ PlutusScriptRef refScript
                }
            ]
        , balancer: defaultBalancer
        , balancerCtx: emptyBalancerCtx
        }
      awaitTxConfirmed lockTxHash

      scriptUtxos <- utxosAt v1Addr
      utxo <- liftContractM "Could not find locked V1 UTxO"
        $ Array.head
        $ lookupTxHash lockTxHash scriptUtxos

      result <- try do
        logInfo' "Attempting V1 spend of UTxO with attached reference script"
        { txHash } <- submitTxFromBlueprint
          { buildSteps:
              [ SpendOutput utxo
                  ( Just $ PlutusScriptOutput
                      (ScriptValue v1Validator)
                      RedeemerDatum.unit
                      (Just $ DatumValue datumUnit)
                  )
              ]
          , balancer: defaultBalancer
          , balancerCtx:
              { balancerConstraints: mempty
              , extraUtxos: toUtxoMap [ utxo ]
              }
          }
        awaitTxConfirmed txHash
      result `shouldSatisfy` isLeft

  test "10. V1 coexists with datum-less/refscript-less reference inputs" do
    -- CIP-0110. V1 execution is permitted in a tx that has reference
    -- inputs, provided those reference-input UTxOs have neither inline
    -- datums nor reference scripts.
    --
    -- Shape:
    --  * Lock a UTxO under a V1 validator (hashed datum).
    --  * Create a plain payment output to Alice.
    --  * Build a tx that spends the V1-locked UTxO (with a DatumValue
    --    witness) and references the plain output.
    --  * Expect: awaitTxConfirmed succeeds.
    withWallets standardDistribution \alice -> withKeyWallet alice do
      v1Validator <- V1.alwaysSucceedsScript
      v1Addr <- mkAddress
        (wrap $ ScriptHashCredential $ PlutusScript.hash v1Validator)
        Nothing
      let
        datumUnit = PlutusData.unit
        dHash = hashPlutusData datumUnit

      logInfo' "Locking value at V1 address with hashed datum"
      { txHash: lockTxHash } <- submitTxFromBlueprint
        { buildSteps:
            [ Pay $ TransactionOutput
                { address: v1Addr
                , amount: Value.lovelaceValueOf $ BigNum.fromInt 3_000_000
                , datum: Just $ OutputDatumHash dHash
                , scriptRef: Nothing
                }
            ]
        , balancer: defaultBalancer
        , balancerCtx: emptyBalancerCtx
        }
      awaitTxConfirmed lockTxHash

      scriptUtxos <- utxosAt v1Addr
      scriptUtxo <- liftContractM "Could not find locked V1 UTxO"
        $ Array.head
        $ lookupTxHash lockTxHash scriptUtxos

      refUtxo <- pickWalletUtxo
      let refOref = (unwrap refUtxo).input

      logInfo' "Spending V1 UTxO with a plain reference input"
      txHash <- submitTxWithEdit
        (addReferenceInputs [ refOref ])
        { buildSteps:
            [ SpendOutput scriptUtxo
                ( Just $ PlutusScriptOutput
                    (ScriptValue v1Validator)
                    RedeemerDatum.unit
                    (Just $ DatumValue datumUnit)
                )
            ]
        , balancer: defaultBalancer
        , balancerCtx:
            { balancerConstraints: mustNotSpendUtxoWithOutRef refOref
            , extraUtxos: toUtxoMap [ scriptUtxo, refUtxo ]
            }
        }
      awaitTxConfirmed txHash

  test "11. V1 script invoked via reference script" do
    -- Checks whether a V1 validator can satisfy a script credential when
    -- provided as a `scriptRef` on a reference-input UTxO instead of in
    -- the witness set.
    --
    -- Shape:
    --  * Deploy a UTxO carrying the V1 script as `referenceScript`.
    --  * Lock another UTxO at the V1 script address with a hashed datum.
    --  * Spend the V1-locked UTxO using `ScriptReference` pointing at the
    --    ref-script UTxO instead of `ScriptValue` in the witness set.
    --  * Expect: confirmed if V1-as-refscript is supported. Test fails
    --    with the ledger's error if not.
    withWallets standardDistribution \alice -> withKeyWallet alice do
      v1Validator <- V1.alwaysSucceedsScript
      v1Addr <- mkAddress
        (wrap $ ScriptHashCredential $ PlutusScript.hash v1Validator)
        Nothing
      aliceAddr <- liftContractM "No wallet address"
        =<< (Array.head <$> getWalletAddresses)
      let
        datumUnit = PlutusData.unit
        dHash = hashPlutusData datumUnit

      logInfo' "Deploying V1 as ref script and locking a V1 UTxO"
      { txHash: setupTxHash } <- submitTxFromBlueprint
        { buildSteps:
            [ Pay $ TransactionOutput
                { address: aliceAddr
                , amount: Value.lovelaceValueOf $ BigNum.fromInt 5_000_000
                , datum: Nothing
                , scriptRef: Just $ PlutusScriptRef v1Validator
                }
            , Pay $ TransactionOutput
                { address: v1Addr
                , amount: Value.lovelaceValueOf $ BigNum.fromInt 3_000_000
                , datum: Just $ OutputDatumHash dHash
                , scriptRef: Nothing
                }
            ]
        , balancer: defaultBalancer
        , balancerCtx: emptyBalancerCtx
        }
      awaitTxConfirmed setupTxHash

      aliceUtxos <- utxosAt aliceAddr
      refScriptUtxo <- liftContractM "Could not find V1 ref-script UTxO"
        $ Array.find hasV1RefScript
        $ lookupTxHash setupTxHash aliceUtxos
      let refScriptOref = (unwrap refScriptUtxo).input

      v1Utxos <- utxosAt v1Addr
      v1Utxo <- liftContractM "Could not find locked V1 UTxO"
        $ Array.head
        $ lookupTxHash setupTxHash v1Utxos

      logInfo' "Spending V1 UTxO with V1 script provided via reference script"
      { txHash } <- submitTxFromBlueprint
        { buildSteps:
            [ SpendOutput v1Utxo
                ( Just $ PlutusScriptOutput
                    (ScriptReference refScriptOref ReferenceInput)
                    RedeemerDatum.unit
                    (Just $ DatumValue datumUnit)
                )
            ]
        , balancer: defaultBalancer
        , balancerCtx:
            { balancerConstraints:
                mustNotSpendUtxoWithOutRef refScriptOref
            , extraUtxos: toUtxoMap [ v1Utxo, refScriptUtxo ]
            }
        }
      awaitTxConfirmed txHash

  test "12. Paying to a V3 script address without a datum is possible" do
    -- CIP-69. Positive case: builder + ledger accept a V3 script output
    -- with `datum: Nothing`.
    --
    -- Shape:
    --  * Build a tx with a single `Pay` step to a V3 script address,
    --    `datum: Nothing`, `scriptRef: Nothing`.
    --  * Expect: awaitTxConfirmed succeeds.
    withWallets standardDistribution \alice -> withKeyWallet alice do
      validator <- V3.alwaysSucceedsScriptV3
      scriptAddr <- mkAddress
        (wrap $ ScriptHashCredential $ PlutusScript.hash validator)
        Nothing
      { txHash } <- submitTxFromBlueprint
        { buildSteps:
            [ Pay $ TransactionOutput
                { address: scriptAddr
                , amount: Value.lovelaceValueOf $ BigNum.fromInt 2_000_000
                , datum: Nothing
                , scriptRef: Nothing
                }
            ]
        , balancer: defaultBalancer
        , balancerCtx: emptyBalancerCtx
        }
      awaitTxConfirmed txHash

  test
    "13. Paying to a V1/V2 script address without a datum is also accepted"
    do
      -- Documents the over-permissive trap: the builder does not gate
      -- on address language, so this creates unspendable dead value.
      -- Test only verifies the tx is *built and confirmed*. We do not
      -- attempt to spend it back.
      --
      -- Shape:
      --  * Build a tx with a single `Pay` step to a V2 script address,
      --    `datum: Nothing`, `scriptRef: Nothing`.
      --  * Expect: awaitTxConfirmed succeeds.
      withWallets standardDistribution \alice -> withKeyWallet alice do
        validator <- V2.alwaysSucceedsScriptV2
        scriptAddr <- mkAddress
          (wrap $ ScriptHashCredential $ PlutusScript.hash validator)
          Nothing
        { txHash } <- submitTxFromBlueprint
          { buildSteps:
              [ Pay $ TransactionOutput
                  { address: scriptAddr
                  , amount: Value.lovelaceValueOf $ BigNum.fromInt 2_000_000
                  , datum: Nothing
                  , scriptRef: Nothing
                  }
              ]
          , balancer: defaultBalancer
          , balancerCtx: emptyBalancerCtx
          }
        awaitTxConfirmed txHash

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

standardDistribution :: InitialUTxOs
standardDistribution =
  [ BigNum.fromInt 5_000_000
  , BigNum.fromInt 5_000_000
  , BigNum.fromInt 5_000_000
  , BigNum.fromInt 50_000_000
  ]

-- Pick an arbitrary UTxO owned by the currently active wallet.
pickWalletUtxo :: Contract TransactionUnspentOutput
pickWalletUtxo = do
  utxos <- fromMaybe Map.empty <$> getWalletUtxos
  input /\ output <-
    liftContractM "pickWalletUtxo: wallet has no UTxOs"
      $ Array.head (Map.toUnfoldable utxos)
  pure $ TransactionUnspentOutput { input, output }

-- Like `pickWalletUtxo` but avoids the given input.
pickWalletUtxoExcluding
  :: TransactionInput -> Contract TransactionUnspentOutput
pickWalletUtxoExcluding excluded = do
  utxos <- fromMaybe Map.empty <$> getWalletUtxos
  input /\ output <-
    liftContractM "pickWalletUtxoExcluding: no eligible wallet UTxOs"
      $ Array.head
      $ Array.filter (\(oref /\ _) -> oref /= excluded)
      $ Map.toUnfoldable utxos
  pure $ TransactionUnspentOutput { input, output }

-- Like `submitTxFromBlueprint`, but applies `editTransaction` to the built
-- tx before balancing. Use this when the shape you need cannot be expressed
-- via `TransactionBuilderStep` alone (e.g. bare reference inputs).
submitTxWithEdit
  :: forall ctx
   . (Transaction -> Transaction)
  -> TxBlueprint ctx
  -> Contract TransactionHash
submitTxWithEdit editFn blueprint = do
  unbalancedTx <- buildTx blueprint.buildSteps
  let edited = editTransaction editFn unbalancedTx
  balancedTx <- liftEither =<< blueprint.balancer edited blueprint.balancerCtx
  signed <- signTransaction balancedTx
  submit signed

-- Append the given inputs to the tx's reference-input list.
addReferenceInputs
  :: Array TransactionInput -> Transaction -> Transaction
addReferenceInputs refs =
  _body <<< _referenceInputs %~ (_ <> refs)

-- Does this UTxO carry a V1 Plutus script as its `scriptRef`?
hasV1RefScript :: TransactionUnspentOutput -> Boolean
hasV1RefScript utxo = case (unwrap (unwrap utxo).output).scriptRef of
  Just (PlutusScriptRef ps) ->
    let
      PlutusScript (_ /\ lang) = ps
    in
      lang == PlutusV1
  _ -> false

-- Pay to the given script's address with `datum: Nothing`, then assert
-- that spending the resulting UTxO with that same script fails. Used by
-- item 8 to cover the V1 and V2 paths uniformly.
assertDatumLessSpendFails :: PlutusScript -> Contract Unit
assertDatumLessSpendFails validator = do
  scriptAddr <- mkAddress
    (wrap $ ScriptHashCredential $ PlutusScript.hash validator)
    Nothing

  logInfo' "Sending value to script address without a datum"
  { txHash: lockTxHash } <- submitTxFromBlueprint
    { buildSteps:
        [ Pay $ TransactionOutput
            { address: scriptAddr
            , amount: Value.lovelaceValueOf $ BigNum.fromInt 3_000_000
            , datum: Nothing
            , scriptRef: Nothing
            }
        ]
    , balancer: defaultBalancer
    , balancerCtx: emptyBalancerCtx
    }
  awaitTxConfirmed lockTxHash

  scriptUtxos <- utxosAt scriptAddr
  scriptUtxo <- liftContractM "Could not find datum-less script UTxO"
    $ Array.head
    $ lookupTxHash lockTxHash scriptUtxos

  result <- try do
    logInfo' "Attempting to spend datum-less script UTxO"
    { txHash } <- submitTxFromBlueprint
      { buildSteps:
          [ SpendOutput scriptUtxo
              ( Just $ PlutusScriptOutput
                  (ScriptValue validator)
                  RedeemerDatum.unit
                  Nothing
              )
          ]
      , balancer: defaultBalancer
      , balancerCtx:
          { balancerConstraints: mempty
          , extraUtxos: toUtxoMap [ scriptUtxo ]
          }
      }
    awaitTxConfirmed txHash
  result `shouldSatisfy` isLeft
