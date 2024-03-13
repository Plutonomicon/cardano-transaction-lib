module Ctl.Internal.Lens
  ( _amount
  , _auxiliaryData
  , _auxiliaryDataHash
  , _body
  , _certs
  , _collateral
  , _collateralReturn
  , _fee
  , _inputs
  , _isValid
  , _mint
  , _networkId
  , _outputs
  , _plutusData
  , _plutusScripts
  , _redeemers
  , _referenceInputs
  , _requiredSigners
  , _scriptDataHash
  , _totalCollateral
  , _vkeys
  , _withdrawals
  , _witnessSet
  , _datum
  , _scriptRef
  ) where

import Prelude

import Cardano.Types
  ( AuxiliaryData
  , AuxiliaryDataHash
  , Certificate
  , Coin
  , Ed25519KeyHash
  , Mint
  , NetworkId
  , OutputDatum
  , PlutusData
  , PlutusScript
  , Redeemer
  , ScriptDataHash
  , ScriptRef
  , Transaction
  , TransactionBody
  , TransactionInput
  , TransactionOutput
  , TransactionWitnessSet
  , Value
  , Vkeywitness
  )
import Cardano.Types.RewardAddress (RewardAddress)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Type.Proxy (Proxy(Proxy))

-- Transaction

_body :: Lens' Transaction TransactionBody
_body = _Newtype <<< prop (Proxy :: Proxy "body")

_isValid :: Lens' Transaction Boolean
_isValid = _Newtype <<< prop (Proxy :: Proxy "isValid")

_witnessSet :: Lens' Transaction TransactionWitnessSet
_witnessSet = _Newtype <<< prop (Proxy :: Proxy "witnessSet")

_auxiliaryData :: Lens' Transaction AuxiliaryData
_auxiliaryData = _Newtype <<< prop (Proxy :: Proxy "auxiliaryData")

-- TransactionBody

_inputs :: Lens' TransactionBody (Array TransactionInput)
_inputs = _Newtype <<< prop (Proxy :: Proxy "inputs")

_fee :: Lens' TransactionBody Coin
_fee = _Newtype <<< prop (Proxy :: Proxy "fee")

_outputs :: Lens' TransactionBody (Array TransactionOutput)
_outputs = _Newtype <<< prop (Proxy :: Proxy "outputs")

_certs :: Lens' TransactionBody (Array Certificate)
_certs = _Newtype <<< prop (Proxy :: Proxy "certs")

_networkId :: Lens' TransactionBody (Maybe NetworkId)
_networkId = _Newtype <<< prop (Proxy :: Proxy "networkId")

_scriptDataHash :: Lens' TransactionBody (Maybe ScriptDataHash)
_scriptDataHash = _Newtype <<< prop (Proxy :: Proxy "scriptDataHash")

_collateral :: Lens' TransactionBody (Array TransactionInput)
_collateral = _Newtype <<< prop (Proxy :: Proxy "collateral")

_collateralReturn :: Lens' TransactionBody (Maybe TransactionOutput)
_collateralReturn = _Newtype <<< prop (Proxy :: Proxy "collateralReturn")

_totalCollateral :: Lens' TransactionBody (Maybe Coin)
_totalCollateral = _Newtype <<< prop (Proxy :: Proxy "totalCollateral")

_referenceInputs :: Lens' TransactionBody (Array TransactionInput)
_referenceInputs = _Newtype <<< prop (Proxy :: Proxy "referenceInputs")

_requiredSigners :: Lens' TransactionBody (Array Ed25519KeyHash)
_requiredSigners = _Newtype <<< prop (Proxy :: Proxy "requiredSigners")

_withdrawals :: Lens' TransactionBody (Map RewardAddress Coin)
_withdrawals = _Newtype <<< prop (Proxy :: Proxy "withdrawals")

_mint :: Lens' TransactionBody (Maybe Mint)
_mint = _Newtype <<< prop (Proxy :: Proxy "mint")

_auxiliaryDataHash :: Lens' TransactionBody (Maybe AuxiliaryDataHash)
_auxiliaryDataHash = _Newtype <<< prop (Proxy :: Proxy "auxiliaryDataHash")

-- TransactionOutput

_amount :: Lens' TransactionOutput Value
_amount = _Newtype <<< prop (Proxy :: Proxy "amount")

_scriptRef :: Lens' TransactionOutput (Maybe ScriptRef)
_scriptRef = _Newtype <<< prop (Proxy :: Proxy "scriptRef")

_datum :: Lens' TransactionOutput (Maybe OutputDatum)
_datum = _Newtype <<< prop (Proxy :: Proxy "datum")

-- TransactionWitnessSet

_redeemers :: Lens' TransactionWitnessSet (Array Redeemer)
_redeemers = _Newtype <<< prop (Proxy :: Proxy "redeemers")

_plutusData :: Lens' TransactionWitnessSet (Array PlutusData)
_plutusData = _Newtype <<< prop (Proxy :: Proxy "plutusData")

_plutusScripts :: Lens' TransactionWitnessSet (Array PlutusScript)
_plutusScripts = _Newtype <<< prop (Proxy :: Proxy "plutusScripts")

_vkeys :: Lens' TransactionWitnessSet (Array Vkeywitness)
_vkeys = _Newtype <<< prop (Proxy :: Proxy "vkeys")
