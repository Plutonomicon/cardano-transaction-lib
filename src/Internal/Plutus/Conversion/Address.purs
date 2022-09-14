module CTL.Internal.Plutus.Conversion.Address
  ( fromPlutusAddress
  , fromPlutusAddressWithNetworkTag
  , toPlutusAddress
  , toPlutusAddressWithNetworkTag
  ) where

import Prelude

import CTL.Internal.Plutus.Types.Address
  ( Address(Address)
  , AddressWithNetworkTag(AddressWithNetworkTag)
  ) as Plutus
import CTL.Internal.Plutus.Types.Credential
  ( Credential(PubKeyCredential, ScriptCredential)
  , StakingCredential(StakingHash, StakingPtr)
  )
import CTL.Internal.Serialization.Address
  ( Address
  , NetworkId
  , Pointer
  , StakeCredential
  , addressNetworkId
  , baseAddressDelegationCred
  , baseAddressFromAddress
  , baseAddressPaymentCred
  , baseAddressToAddress
  , enterpriseAddressFromAddress
  , enterpriseAddressPaymentCred
  , enterpriseAddressToAddress
  , paymentKeyHashEnterpriseAddress
  , paymentKeyHashPointerAddress
  , paymentKeyHashScriptHashAddress
  , paymentKeyHashStakeKeyHashAddress
  , pointerAddressFromAddress
  , pointerAddressPaymentCred
  , pointerAddressStakePointer
  , pointerAddressToAddress
  , scriptHashEnterpriseAddress
  , scriptHashPointerAddress
  , scriptHashScriptHashAddress
  , scriptHashStakeKeyHashAddress
  , withStakeCredential
  ) as Csl
import CTL.Internal.Types.PubKeyHash (PubKeyHash(PubKeyHash))
import CTL.Internal.Types.Scripts (ValidatorHash(ValidatorHash))
import Control.Alt ((<|>))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)

--------------------------------------------------------------------------------
-- Plutus Address -> CSL Address
--------------------------------------------------------------------------------

fromPlutusAddressWithNetworkTag
  :: Plutus.AddressWithNetworkTag -> Csl.Address
fromPlutusAddressWithNetworkTag (Plutus.AddressWithNetworkTag rec) =
  fromPlutusAddress rec.networkId rec.address

-- | Builds a CSL-level address from a Plutus address.
-- | CIP-0019: https://cips.cardano.org/cips/cip19/
fromPlutusAddress
  :: Csl.NetworkId -> Plutus.Address -> Csl.Address
fromPlutusAddress
  networkId
  (Plutus.Address { addressCredential, addressStakingCredential }) =
  case addressCredential, addressStakingCredential of
    -- %b0000 | network tag | key hash | key hash
    PubKeyCredential (PubKeyHash pkh),
    Just (StakingHash (PubKeyCredential (PubKeyHash skh))) ->
      Csl.baseAddressToAddress $
        Csl.paymentKeyHashStakeKeyHashAddress networkId pkh skh

    -- %b0001 | network tag | script hash | key hash
    ScriptCredential (ValidatorHash sh),
    Just (StakingHash (PubKeyCredential (PubKeyHash skh))) ->
      Csl.baseAddressToAddress $
        Csl.scriptHashStakeKeyHashAddress networkId sh skh

    -- %b0010 | network tag | key hash | script hash
    PubKeyCredential (PubKeyHash pkh),
    Just (StakingHash (ScriptCredential (ValidatorHash sh))) ->
      Csl.baseAddressToAddress $
        Csl.paymentKeyHashScriptHashAddress networkId pkh sh

    -- %b0011 | network tag | script hash | script hash
    ScriptCredential (ValidatorHash sh),
    Just (StakingHash (ScriptCredential (ValidatorHash sh'))) ->
      Csl.baseAddressToAddress $
        Csl.scriptHashScriptHashAddress networkId sh sh'

    -- %b0100 | network tag | key hash | pointer
    PubKeyCredential (PubKeyHash pkh), Just (StakingPtr ptr) ->
      Csl.pointerAddressToAddress $
        Csl.paymentKeyHashPointerAddress networkId pkh ptr

    -- %b0101 | network tag | script hash | pointer
    ScriptCredential (ValidatorHash sh), Just (StakingPtr ptr) ->
      Csl.pointerAddressToAddress $
        Csl.scriptHashPointerAddress networkId sh ptr

    -- %b0110 | network tag | key hash
    PubKeyCredential (PubKeyHash pkh), Nothing ->
      Csl.enterpriseAddressToAddress $
        Csl.paymentKeyHashEnterpriseAddress networkId pkh

    -- %b0111 | network tag | script hash
    ScriptCredential (ValidatorHash sh), Nothing ->
      Csl.enterpriseAddressToAddress $
        Csl.scriptHashEnterpriseAddress networkId sh

--------------------------------------------------------------------------------
-- CSL Address -> Plutus Address
--------------------------------------------------------------------------------

-- | Attempts to build a Plutus address from a CSL-level address
-- | discarding the network tag.
-- | CIP-0019: https://cips.cardano.org/cips/cip19/
toPlutusAddress
  :: Csl.Address -> Maybe Plutus.Address
toPlutusAddress =
  map (_.address <<< unwrap) <<< toPlutusAddressWithNetworkTag

-- | Attempts to build a Plutus address from a CSL-level address.
-- | CIP-0019: https://cips.cardano.org/cips/cip19/
toPlutusAddressWithNetworkTag
  :: Csl.Address -> Maybe Plutus.AddressWithNetworkTag
toPlutusAddressWithNetworkTag addressCsl = do
  let networkId = Csl.addressNetworkId addressCsl
  Plutus.AddressWithNetworkTag <<< { address: _, networkId } <$>
    ( toPlutusBaseAddress addressCsl
        <|> toPlutusPointerAddress addressCsl
        <|> toPlutusEnterpriseAddress addressCsl
    )
  where
  toPlutusBaseAddress
    :: Csl.Address -> Maybe Plutus.Address
  toPlutusBaseAddress addr = do
    baseAddress <- Csl.baseAddressFromAddress addr
    let
      paymentCred :: Csl.StakeCredential
      paymentCred = Csl.baseAddressPaymentCred baseAddress

      delegationCred :: Csl.StakeCredential
      delegationCred = Csl.baseAddressDelegationCred baseAddress

    flip Csl.withStakeCredential paymentCred
      { onKeyHash: \pkh ->
          worker (PubKeyCredential (wrap pkh)) delegationCred
      , onScriptHash: \sh ->
          worker (ScriptCredential (wrap sh)) delegationCred
      }
    where
    worker
      :: Credential -> Csl.StakeCredential -> Maybe Plutus.Address
    worker addressCredential delegationCred =
      Just $ wrap
        { addressCredential
        , addressStakingCredential:
            flip Csl.withStakeCredential delegationCred
              { onKeyHash: \skh ->
                  Just (StakingHash (PubKeyCredential (wrap skh)))
              , onScriptHash: \sh ->
                  Just (StakingHash (ScriptCredential (wrap sh)))
              }
        }

  toPlutusPointerAddress
    :: Csl.Address -> Maybe Plutus.Address
  toPlutusPointerAddress addr = do
    pointerAddress <- Csl.pointerAddressFromAddress addr
    let
      paymentCred :: Csl.StakeCredential
      paymentCred = Csl.pointerAddressPaymentCred pointerAddress

      stakePointer :: Csl.Pointer
      stakePointer = Csl.pointerAddressStakePointer pointerAddress

    Just $ wrap
      { addressCredential: mkAddressCredential paymentCred
      , addressStakingCredential: Just (StakingPtr stakePointer)
      }

  toPlutusEnterpriseAddress
    :: Csl.Address -> Maybe Plutus.Address
  toPlutusEnterpriseAddress addr = do
    enterpriseAddress <- Csl.enterpriseAddressFromAddress addr
    let
      paymentCred :: Csl.StakeCredential
      paymentCred = Csl.enterpriseAddressPaymentCred enterpriseAddress

    Just $ wrap
      { addressCredential: mkAddressCredential paymentCred
      , addressStakingCredential: Nothing
      }

  mkAddressCredential
    :: Csl.StakeCredential -> Credential
  mkAddressCredential =
    Csl.withStakeCredential
      { onKeyHash: PubKeyCredential <<< wrap
      , onScriptHash: ScriptCredential <<< wrap
      }
