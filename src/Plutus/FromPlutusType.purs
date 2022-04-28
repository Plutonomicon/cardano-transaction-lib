module Plutus.FromPlutusType
  ( class FromPlutusType
  , fromPlutusType
  ) where

import Prelude

import Data.Array (head, partition, singleton)
import Data.Identity (Identity(Identity))
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, fromJust)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\))
import Data.UInt (UInt, fromInt, toInt, (.&.), (.|.), shl, zshr)
import Partial.Unsafe (unsafePartial)

import Serialization.Address (Address) as Serialization
import Serialization.Address (Pointer, addressFromBytes) as Serialization.Address
import Serialization.Hash (ed25519KeyHashToBytes, scriptHashToBytes)

import Plutus.Types.Address (Address) as Plutus
import Plutus.Types.AddressHeaderType
  ( AddressHeaderType(..)
  , addrHeaderTypeUInt
  )
import Plutus.Types.Credential
  ( Credential(PubKeyCredential, ScriptCredential)
  , StakingCredential(StakingHash, StakingPtr)
  )
import Plutus.Types.AssocMap (lookup) as Plutus.AssocMap
import Plutus.Types.CurrencySymbol (adaSymbol, getCurrencySymbol) as Plutus
import Plutus.Types.Value (Value) as Plutus
import Plutus.Types.Value (getValue) as Plutus.Value

import Types.ByteArray (ByteArray, byteArrayFromIntArrayUnsafe)
import Types.TokenName (adaToken)
import Types.Value (Value) as Types
import Types.Value (NonAdaAsset, mkValue, mkNonAdaAssetsFromTokenMap)

class FromPlutusType :: (Type -> Type) -> Type -> Type -> Constraint
class FromPlutusType f pt t | pt -> t, t pt -> f where
  fromPlutusType :: pt -> f t

--------------------------------------------------------------------------------
-- Plutus.Types.Value -> Types.Value
--------------------------------------------------------------------------------

-- The underlying `Plutus.Types.AssocMap` of `Plutus.Types.Value` doesn't
-- have the `Ord` constraint on the keys. Therefore, one should be careful when
-- performing conversions between `Value`s, since the ordering of components
-- can't be guaranteed.
instance FromPlutusType Identity Plutus.Value Types.Value where
  fromPlutusType plutusValue =
    Identity (adaValue <> mkValue mempty nonAdaAssets)
    where
    { adaTokenMap, nonAdaTokenMap } =
      (\x -> { adaTokenMap: x.yes, nonAdaTokenMap: x.no }) <<<
        partition (\(cs /\ _) -> cs == Plutus.adaSymbol) $
        (unwrap $ Plutus.Value.getValue plutusValue)

    adaValue :: Types.Value
    adaValue = flip mkValue mempty <<< wrap <<< fromMaybe zero $ do
      adaTokens <- Tuple.snd <$> head adaTokenMap
      Plutus.AssocMap.lookup adaToken adaTokens

    nonAdaAssets :: NonAdaAsset
    nonAdaAssets = unsafePartial $ fromJust
      $ mkNonAdaAssetsFromTokenMap
      $ nonAdaTokenMap <#> \(cs /\ tokens) ->
          Plutus.getCurrencySymbol cs /\ Map.fromFoldable (unwrap tokens)

--------------------------------------------------------------------------------
-- Plutus.Types.Address -> Maybe Serialization.Address
--------------------------------------------------------------------------------

instance FromPlutusType Maybe Plutus.Address Serialization.Address where
  -- | Attempts to build a CSL-level address from a Plutus address
  -- | based on the CIP-0019.
  fromPlutusType addrPlutus =
    case rec.addressCredential, rec.addressStakingCredential of
      -- %b0000 | network tag | key hash | key hash
      PubKeyCredential pkh, Just (StakingHash (PubKeyCredential skh)) ->
        Serialization.Address.addressFromBytes $
          addrHeader PaymentKeyHashStakeKeyHash
            <> ed25519KeyHashToBytes (unwrap pkh)
            <> ed25519KeyHashToBytes (unwrap skh)

      -- %b0001 | network tag | script hash | key hash
      ScriptCredential sh, Just (StakingHash (PubKeyCredential skh)) ->
        Serialization.Address.addressFromBytes $
          addrHeader ScriptHashStakeKeyHash
            <> scriptHashToBytes (unwrap sh)
            <> ed25519KeyHashToBytes (unwrap skh)

      -- %b0010 | network tag | key hash | script hash
      PubKeyCredential pkh, Just (StakingHash (ScriptCredential sh)) ->
        Serialization.Address.addressFromBytes $
          addrHeader PaymentKeyHashScriptHash
            <> ed25519KeyHashToBytes (unwrap pkh)
            <> scriptHashToBytes (unwrap sh)

      -- %b0011 | network tag | script hash | script hash
      ScriptCredential sh, Just (StakingHash (ScriptCredential sh')) ->
        Serialization.Address.addressFromBytes $
          addrHeader ScriptHashScriptHash
            <> scriptHashToBytes (unwrap sh)
            <> scriptHashToBytes (unwrap sh')

      -- %b0100 | network tag | key hash | pointer
      PubKeyCredential pkh, Just (StakingPtr ptr) ->
        Serialization.Address.addressFromBytes $
          addrHeader PaymentKeyHashPointer
            <> ed25519KeyHashToBytes (unwrap pkh)
            <> pointerToBytes ptr

      -- %b0101 | network tag | script hash | pointer
      ScriptCredential sh, Just (StakingPtr ptr) ->
        Serialization.Address.addressFromBytes $
          addrHeader ScriptHashPointer
            <> scriptHashToBytes (unwrap sh)
            <> pointerToBytes ptr

      -- %b0110 | network tag | key hash
      PubKeyCredential pkh, Nothing ->
        Serialization.Address.addressFromBytes $
          addrHeader PaymentKeyHash
            <> ed25519KeyHashToBytes (unwrap pkh)

      -- %b0111 | network tag | script hash
      ScriptCredential sh, Nothing ->
        Serialization.Address.addressFromBytes $
          addrHeader ScriptHash
            <> scriptHashToBytes (unwrap sh)
    where
    rec
      :: { addressCredential :: Credential
         , addressStakingCredential :: Maybe StakingCredential
         }
    rec = unwrap addrPlutus

    -- | Encodes the address type along with the network tag (%b0001 - Mainnet)
    -- | as a one-element byte array.
    addrHeader :: AddressHeaderType -> ByteArray
    addrHeader headerType =
      byteArrayFromIntArrayUnsafe <<< singleton <<< toInt $
        (addrHeaderTypeUInt headerType `shl` fromInt 4) + fromInt 1

    pointerToBytes :: Serialization.Address.Pointer -> ByteArray
    pointerToBytes ptr =
      toVarLengthUInt ptr.slot <> toVarLengthUInt ptr.txIx <>
        toVarLengthUInt ptr.certIx

-- | Encodes the variable-length positive number as a byte array
-- | according to the ABNF grammar below.
-- |
-- | variable-length-uint =
-- |     (%b1 | uint7 | variable-length-uint)
-- |   / (%b0 | uint7)
-- | uint7 = 7bit
toVarLengthUInt :: forall (t :: Type). Newtype t UInt => t -> ByteArray
toVarLengthUInt t = worker (unwrap t) false
  where
  worker :: UInt -> Boolean -> ByteArray
  worker srcUInt setSignalBit
    | srcUInt == zero = mempty
    | otherwise =
        let
          -- Apply bit mask (127 = %b01111111) to extract the 7-bit component.
          uint7 = srcUInt .&. (fromInt 127)
          -- Right shift the var-length number by 7 bit positions to
          -- prepare the source number for the next 7-bit component extraction.
          srcUIntShifted = srcUInt `zshr` fromInt 7
        in
          append (worker srcUIntShifted true)
            $ byteArrayFromIntArrayUnsafe <<< singleton <<< toInt
            -- Turn off the signal bit for the last 7-bit component in the array.
            $ if setSignalBit then uint7 .|. fromInt 128 else uint7
