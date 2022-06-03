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
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple), snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.UInt (UInt, fromInt, toInt, (.&.), (.|.), shl, zshr)
import Partial.Unsafe (unsafePartial)

import Serialization.Address (Address) as Serialization
import Serialization.Address (Pointer, addressFromBytes) as Serialization.Address
import Serialization.Address (NetworkId, networkIdtoInt)
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
import Plutus.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  , UtxoM(UtxoM)
  ) as Plutus
import Plutus.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  ) as Plutus
import Plutus.Types.Value (Coin, Value) as Plutus
import Plutus.Types.Value (getValue) as Plutus.Value

import Types.ByteArray (ByteArray, byteArrayFromIntArrayUnsafe)
import Types.CborBytes
  ( CborBytes
  , cborBytesFromIntArrayUnsafe
  , rawBytesAsCborBytes
  )
import Types.TokenName (adaToken)
import Cardano.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  , UtxoM(UtxoM)
  ) as Cardano
import Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  ) as Cardano
import Cardano.Types.Value (Coin, Value) as Types
import Cardano.Types.Value (NonAdaAsset, mkValue, mkNonAdaAssetsFromTokenMap)

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
      adaTokens <- snd <$> head adaTokenMap
      Plutus.AssocMap.lookup adaToken adaTokens

    nonAdaAssets :: NonAdaAsset
    nonAdaAssets = unsafePartial $ fromJust
      $ mkNonAdaAssetsFromTokenMap
      $ nonAdaTokenMap <#> \(cs /\ tokens) ->
          Plutus.getCurrencySymbol cs /\ Map.fromFoldable (unwrap tokens)

--------------------------------------------------------------------------------
-- Plutus.Types.Value.UtxoM -> Cardano.Types.Value.Coin
--------------------------------------------------------------------------------

instance FromPlutusType Identity Plutus.Coin Types.Coin where
  fromPlutusType = pure <<< wrap <<< unwrap

--------------------------------------------------------------------------------
-- Plutus.Types.Address -> Maybe Serialization.Address
--------------------------------------------------------------------------------

instance
  FromPlutusType Maybe (NetworkId /\ Plutus.Address) Serialization.Address where
  -- | Attempts to build a CSL-level address from a Plutus address
  -- | based on the CIP-0019.
  fromPlutusType (networkId /\ addrPlutus) = do
    case rec.addressCredential, rec.addressStakingCredential of
      -- %b0000 | network tag | key hash | key hash
      PubKeyCredential pkh, Just (StakingHash (PubKeyCredential skh)) ->
        Serialization.Address.addressFromBytes $
          addrHeader PaymentKeyHashStakeKeyHash
            <> rawBytesAsCborBytes (ed25519KeyHashToBytes (unwrap pkh))
            <> rawBytesAsCborBytes (ed25519KeyHashToBytes (unwrap skh))

      -- %b0001 | network tag | script hash | key hash
      ScriptCredential sh, Just (StakingHash (PubKeyCredential skh)) ->
        Serialization.Address.addressFromBytes $
          addrHeader ScriptHashStakeKeyHash
            <> rawBytesAsCborBytes (scriptHashToBytes (unwrap sh))
            <> rawBytesAsCborBytes (ed25519KeyHashToBytes (unwrap skh))

      -- %b0010 | network tag | key hash | script hash
      PubKeyCredential pkh, Just (StakingHash (ScriptCredential sh)) ->
        Serialization.Address.addressFromBytes $
          addrHeader PaymentKeyHashScriptHash
            <> rawBytesAsCborBytes (ed25519KeyHashToBytes (unwrap pkh))
            <> rawBytesAsCborBytes (scriptHashToBytes (unwrap sh))

      -- %b0011 | network tag | script hash | script hash
      ScriptCredential sh, Just (StakingHash (ScriptCredential sh')) ->
        Serialization.Address.addressFromBytes $
          addrHeader ScriptHashScriptHash
            <> rawBytesAsCborBytes (scriptHashToBytes (unwrap sh))
            <> rawBytesAsCborBytes (scriptHashToBytes (unwrap sh'))

      -- %b0100 | network tag | key hash | pointer
      PubKeyCredential pkh, Just (StakingPtr ptr) ->
        Serialization.Address.addressFromBytes $
          addrHeader PaymentKeyHashPointer
            <> rawBytesAsCborBytes (ed25519KeyHashToBytes (unwrap pkh))
            <> pointerToBytes ptr

      -- %b0101 | network tag | script hash | pointer
      ScriptCredential sh, Just (StakingPtr ptr) ->
        Serialization.Address.addressFromBytes $
          addrHeader ScriptHashPointer
            <> rawBytesAsCborBytes (scriptHashToBytes (unwrap sh))
            <> pointerToBytes ptr

      -- %b0110 | network tag | key hash
      PubKeyCredential pkh, Nothing ->
        Serialization.Address.addressFromBytes $
          addrHeader PaymentKeyHash
            <> rawBytesAsCborBytes (ed25519KeyHashToBytes (unwrap pkh))

      -- %b0111 | network tag | script hash
      ScriptCredential sh, Nothing ->
        Serialization.Address.addressFromBytes $
          addrHeader ScriptHash
            <> rawBytesAsCborBytes (scriptHashToBytes (unwrap sh))
    where
    rec
      :: { addressCredential :: Credential
         , addressStakingCredential :: Maybe StakingCredential
         }
    rec = unwrap addrPlutus

    -- | Encodes the address type along with the network tag
    -- | as a one-element byte array.
    addrHeader :: AddressHeaderType -> CborBytes
    addrHeader headerType =
      cborBytesFromIntArrayUnsafe <<< singleton <<< toInt $
        (addrHeaderTypeUInt headerType `shl` fromInt 4)
          + fromInt (networkIdtoInt networkId)

    pointerToBytes :: Serialization.Address.Pointer -> CborBytes
    pointerToBytes ptr = wrap $
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

--------------------------------------------------------------------------------
-- Plutus.Types.Transaction.TransactionOutput  ->
-- Maybe Cardano.Types.Transaction.TransactionOutput
--------------------------------------------------------------------------------

instance
  FromPlutusType Maybe
    (NetworkId /\ Plutus.TransactionOutput)
    Cardano.TransactionOutput where
  fromPlutusType
    (networkId /\ Plutus.TransactionOutput { address, amount, dataHash }) = do
    address' <- fromPlutusType (networkId /\ address)
    let amount' = unwrap (fromPlutusType amount)
    pure $ Cardano.TransactionOutput
      { address: address', amount: amount', dataHash }

--------------------------------------------------------------------------------
-- Plutus.Types.Transaction.UtxoM -> Maybe Cardano.Types.Transaction.UtxoM
--------------------------------------------------------------------------------

instance FromPlutusType Maybe (NetworkId /\ Plutus.UtxoM) Cardano.UtxoM where
  fromPlutusType (networkId /\ Plutus.UtxoM utxos) =
    Cardano.UtxoM <$> traverse (fromPlutusType <<< (Tuple networkId)) utxos

--------------------------------------------------------------------------------
-- Plutus.Types.Transaction.TransactionUnspentOutput ->
-- Maybe Cardano.Types.Transaction.TransactionUnspentOutput
--------------------------------------------------------------------------------

instance
  FromPlutusType Maybe
    (NetworkId /\ Plutus.TransactionUnspentOutput)
    Cardano.TransactionUnspentOutput where
  fromPlutusType
    (networkId /\ Plutus.TransactionUnspentOutput { input, output }) = do
    pOutput <- fromPlutusType (networkId /\ output)
    pure $ Cardano.TransactionUnspentOutput { input, output: pOutput }
