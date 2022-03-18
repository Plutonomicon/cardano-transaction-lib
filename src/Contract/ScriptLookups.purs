-- | A module for creating off-chain script lookups, and an unbalanced
-- | transaction.
-- |
-- | The lookup functions may come in pairs. If the function cannot fail, there
-- | is another version contained in a `Maybe` context (that also does not fail).
-- | This is to aid users who wish to utilise the underlying `ScriptLookups`
-- | `Monoid` for `foldMap` etc.
-- |
-- | Otherwise, there will be just one version that can fail (because of
-- | hashing)
module Contract.ScriptLookups (module ScriptLookups) where

import Types.ScriptLookups
  ( MkUnbalancedTxError(..) -- A lot errors so will refrain from explicit names.
  , ScriptLookups(ScriptLookups)
  , generalise
  , mintingPolicyM
  , mkUnbalancedTx
  , otherDataM
  , otherScriptM
  , ownPaymentPubKeyHash
  , ownPaymentPubKeyHashM
  , ownStakePubKeyHash
  , ownStakePubKeyHashM
  , paymentPubKeyM
  , typedValidatorLookups
  , typedValidatorLookupsM
  , unspentOutputs
  , unspentOutputsM
  ) as ScriptLookups