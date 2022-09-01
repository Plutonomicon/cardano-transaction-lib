-- | A module for building `TxConstraints` te pair with the `ScriptLookups`
-- | as part of an off-chain transaction.
module Contract.TxConstraints (module TxConstraints) where

import Types.TxConstraints
  ( InputConstraint(InputConstraint)
  , OutputConstraint(OutputConstraint)
  , TxConstraint
      ( MustIncludeDatum
      , MustValidateIn
      , MustBeSignedBy
      , MustSpendAtLeast
      , MustProduceAtLeast
      , MustSpendPubKeyOutput
      , MustSpendScriptOutput
      , MustMintValue
      , MustPayToPubKeyAddress
      , MustPayToScript
      , MustHashDatum
      , MustSatisfyAnyOf
      , MustNotBeValid
      )
  , TxConstraints(TxConstraints)
  , addTxIn
  , isSatisfiable
  , modifiesUtxoSet
  , mustBeSignedBy
  , mustHashDatum
  , mustIncludeDatum
  , mustMintCurrency
  , mustMintCurrencyWithRedeemer
  , mustMintValue
  , mustMintValueWithRedeemer
  , mustPayToScript
  , mustPayToPubKey
  , mustPayToPubKeyAddress
  , mustPayWithDatumToPubKey
  , mustPayWithDatumToPubKeyAddress
  , mustProduceAtLeast
  , mustProduceAtLeastTotal
  , mustSatisfyAnyOf
  , mustSpendAtLeast
  , mustSpendAtLeastTotal
  , mustSpendPubKeyOutput
  , mustSpendScriptOutput
  , mustNotBeValid
  , mustValidateIn
  , pubKeyPayments
  , requiredDatums
  , requiredMonetaryPolicies
  , requiredSignatories
  , singleton
  ) as TxConstraints
