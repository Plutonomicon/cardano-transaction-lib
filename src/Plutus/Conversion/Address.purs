module Plutus.Conversion.Address
  ( fromPlutusAddress
  , fromPlutusAddressWithNetworkTag
  , toPlutusAddress
  , toPlutusAddressWithNetworkTag
  ) where

import Prelude

import Data.Array (head, singleton, take, drop, foldr, snoc, uncons)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Tuple (fst) as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt, fromInt, toInt, (.&.), and, (.|.), shl, zshr)

import Serialization.Address (Address) as CSL
import Serialization.Address
  ( Pointer
  , addressBytes
  , addressFromBytes
  ) as CSL.Address
import Serialization.Address (NetworkId, networkIdtoInt, unsafeIntToNetId)
import Serialization.Hash
  ( ed25519KeyHashFromBytes
  , ed25519KeyHashToBytes
  , scriptHashFromBytes
  , scriptHashToBytes
  )

import Plutus.Types.Address
  ( Address(Address)
  , AddressWithNetworkTag(AddressWithNetworkTag)
  ) as Plutus
import Plutus.Types.AddressHeaderType
  ( AddressHeaderType
      ( PaymentKeyHashStakeKeyHash
      , ScriptHashStakeKeyHash
      , PaymentKeyHashScriptHash
      , ScriptHashScriptHash
      , PaymentKeyHashPointer
      , ScriptHashPointer
      , PaymentKeyHash
      , ScriptHash
      )
  , addrHeaderType
  , addrHeaderTypeUInt
  )
import Plutus.Types.Credential
  ( Credential(PubKeyCredential, ScriptCredential)
  , StakingCredential(StakingHash, StakingPtr)
  )

import Types.ByteArray
  ( ByteArray
  , byteArrayFromIntArray
  , byteArrayFromIntArrayUnsafe
  , byteArrayToIntArray
  )
import Types.CborBytes
  ( CborBytes
  , cborBytesFromIntArrayUnsafe
  , cborBytesToIntArray
  , rawBytesAsCborBytes
  )

--------------------------------------------------------------------------------
-- Plutus Address -> CSL Address
--------------------------------------------------------------------------------

fromPlutusAddressWithNetworkTag
  :: Plutus.AddressWithNetworkTag -> Maybe CSL.Address
fromPlutusAddressWithNetworkTag (Plutus.AddressWithNetworkTag rec) =
  fromPlutusAddress rec.networkId rec.address

-- | Attempts to build a CSL-level address from a Plutus address.
-- | CIP-0019: https://cips.cardano.org/cips/cip19/
fromPlutusAddress
  :: NetworkId -> Plutus.Address -> Maybe CSL.Address
fromPlutusAddress networkId addrPlutus@(Plutus.Address rec) =
  case rec.addressCredential, rec.addressStakingCredential of
    -- %b0000 | network tag | key hash | key hash
    PubKeyCredential pkh, Just (StakingHash (PubKeyCredential skh)) ->
      CSL.Address.addressFromBytes $
        addrHeader PaymentKeyHashStakeKeyHash
          <> rawBytesAsCborBytes (ed25519KeyHashToBytes (unwrap pkh))
          <> rawBytesAsCborBytes (ed25519KeyHashToBytes (unwrap skh))

    -- %b0001 | network tag | script hash | key hash
    ScriptCredential sh, Just (StakingHash (PubKeyCredential skh)) ->
      CSL.Address.addressFromBytes $
        addrHeader ScriptHashStakeKeyHash
          <> rawBytesAsCborBytes (scriptHashToBytes (unwrap sh))
          <> rawBytesAsCborBytes (ed25519KeyHashToBytes (unwrap skh))

    -- %b0010 | network tag | key hash | script hash
    PubKeyCredential pkh, Just (StakingHash (ScriptCredential sh)) ->
      CSL.Address.addressFromBytes $
        addrHeader PaymentKeyHashScriptHash
          <> rawBytesAsCborBytes (ed25519KeyHashToBytes (unwrap pkh))
          <> rawBytesAsCborBytes (scriptHashToBytes (unwrap sh))

    -- %b0011 | network tag | script hash | script hash
    ScriptCredential sh, Just (StakingHash (ScriptCredential sh')) ->
      CSL.Address.addressFromBytes $
        addrHeader ScriptHashScriptHash
          <> rawBytesAsCborBytes (scriptHashToBytes (unwrap sh))
          <> rawBytesAsCborBytes (scriptHashToBytes (unwrap sh'))

    -- %b0100 | network tag | key hash | pointer
    PubKeyCredential pkh, Just (StakingPtr ptr) ->
      CSL.Address.addressFromBytes $
        addrHeader PaymentKeyHashPointer
          <> rawBytesAsCborBytes (ed25519KeyHashToBytes (unwrap pkh))
          <> pointerToBytes ptr

    -- %b0101 | network tag | script hash | pointer
    ScriptCredential sh, Just (StakingPtr ptr) ->
      CSL.Address.addressFromBytes $
        addrHeader ScriptHashPointer
          <> rawBytesAsCborBytes (scriptHashToBytes (unwrap sh))
          <> pointerToBytes ptr

    -- %b0110 | network tag | key hash
    PubKeyCredential pkh, Nothing ->
      CSL.Address.addressFromBytes $
        addrHeader PaymentKeyHash
          <> rawBytesAsCborBytes (ed25519KeyHashToBytes (unwrap pkh))

    -- %b0111 | network tag | script hash
    ScriptCredential sh, Nothing ->
      CSL.Address.addressFromBytes $
        addrHeader ScriptHash
          <> rawBytesAsCborBytes (scriptHashToBytes (unwrap sh))
  where
  rec
    :: { addressCredential :: Credential
       , addressStakingCredential :: Maybe StakingCredential
       }
  rec = unwrap addrPlutus

  -- Encodes the address type along with the network tag
  -- as a one-element byte array.
  addrHeader :: AddressHeaderType -> CborBytes
  addrHeader headerType =
    cborBytesFromIntArrayUnsafe <<< singleton <<< toInt $
      (addrHeaderTypeUInt headerType `shl` fromInt 4)
        + fromInt (networkIdtoInt networkId)

  pointerToBytes :: CSL.Address.Pointer -> CborBytes
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
-- CSL Address -> Plutus Address
--------------------------------------------------------------------------------

-- | Attempts to build a Plutus address from a CSL-level address
-- | discarding the network tag.
-- | CIP-0019: https://cips.cardano.org/cips/cip19/
toPlutusAddress
  :: CSL.Address -> Maybe Plutus.Address
toPlutusAddress =
  map (_.address <<< unwrap) <<< toPlutusAddressWithNetworkTag

-- | Attempts to build a Plutus address from a CSL-level address.
-- | CIP-0019: https://cips.cardano.org/cips/cip19/
toPlutusAddressWithNetworkTag
  :: CSL.Address -> Maybe Plutus.AddressWithNetworkTag
toPlutusAddressWithNetworkTag addrForeign = do
  headerByte <- head addrBytes
  addrType' <- addrHeaderType (addrType headerByte)
  let networkId = networkTag headerByte
  Plutus.AddressWithNetworkTag <<< { address: _, networkId: networkId } <$>
    case addrType' of
      -- %b0000 | network tag | key hash | key hash
      PaymentKeyHashStakeKeyHash ->
        buildAddress pubKeyCredential $ Just $
          map StakingHash <<< pubKeyCredential

      -- %b0001 | network tag | script hash | key hash
      ScriptHashStakeKeyHash ->
        buildAddress scriptCredential $ Just $
          map StakingHash <<< pubKeyCredential

      -- %b0010 | network tag | key hash | script hash
      PaymentKeyHashScriptHash ->
        buildAddress pubKeyCredential $ Just $
          map StakingHash <<< scriptCredential

      -- %b0011 | network tag | script hash | script hash
      ScriptHashScriptHash ->
        buildAddress scriptCredential $ Just $
          map StakingHash <<< scriptCredential

      -- %b0100 | network tag | key hash | pointer
      PaymentKeyHashPointer ->
        buildAddress pubKeyCredential $ Just stakingPtr

      -- %b0101 | network tag | script hash | pointer
      ScriptHashPointer ->
        buildAddress scriptCredential $ Just stakingPtr

      -- %b0110 | network tag | key hash
      PaymentKeyHash ->
        buildAddress pubKeyCredential Nothing

      -- %b0111 | network tag | script hash
      ScriptHash ->
        buildAddress scriptCredential Nothing
  where
  addrBytes :: Array Int
  addrBytes = cborBytesToIntArray $
    CSL.Address.addressBytes addrForeign

  -- Retrieves the address type by reading
  -- the first 4 bits (from the left) of the header-byte.
  addrType :: Int -> UInt
  addrType = flip zshr (fromInt 4) <<< fromInt

  networkTag :: Int -> NetworkId
  networkTag = unsafeIntToNetId <<< toInt <<< and one <<< fromInt

  -- Retrieves the payment part of the address by reading
  -- the first 28 bytes following the address header.
  paymentPartHash :: Maybe ByteArray
  paymentPartHash = byteArrayFromIntArray $ take 28 (drop 1 addrBytes)

  -- Retrieves the delegation part of the address by reading
  -- the bytes following the payment part.
  delegationPartHash :: Maybe ByteArray
  delegationPartHash = byteArrayFromIntArray $ drop 29 addrBytes

  pubKeyCredential :: ByteArray -> Maybe Credential
  pubKeyCredential =
    map (PubKeyCredential <<< wrap) <<< ed25519KeyHashFromBytes <<< wrap

  scriptCredential :: ByteArray -> Maybe Credential
  scriptCredential =
    map (ScriptCredential <<< wrap) <<< scriptHashFromBytes <<< wrap

  buildAddress
    :: (ByteArray -> Maybe Credential)
    -> Maybe (ByteArray -> Maybe StakingCredential)
    -> Maybe Plutus.Address
  buildAddress credential stakingCredential =
    paymentPartHash >>= credential >>= \c ->
      case stakingCredential of
        Nothing -> Just $
          wrap { addressCredential: c, addressStakingCredential: Nothing }
        Just stakingCred ->
          delegationPartHash >>= stakingCred >>= \sc -> Just $
            wrap { addressCredential: c, addressStakingCredential: Just sc }

  stakingPtr :: ByteArray -> Maybe StakingCredential
  stakingPtr byteArray = do
    slot /\ bytes0 <- fromVarLengthUInt (byteArrayToIntArray byteArray) mempty
    txIx /\ bytes1 <- fromVarLengthUInt bytes0 mempty
    certIx /\ _ <- fromVarLengthUInt bytes1 mempty
    pure $ StakingPtr { slot, txIx, certIx }

-- | Extracts the variable-length positive number from a byte array
-- | according to the ABNF grammar below:
-- |
-- | variable-length-uint =
-- |     (%b1 | uint7 | variable-length-uint)
-- |   / (%b0 | uint7)
-- | uint7 = 7bit
fromVarLengthUInt
  :: forall (t :: Type)
   . Newtype t UInt
  => Array Int
  -> Array UInt
  -> Maybe (t /\ Array Int)
fromVarLengthUInt bytes acc = do
  { head: x', tail: xs } <- uncons bytes
  let x = fromInt x'
  -- Apply bit mask (128 = %b10000000) for inspecting the
  -- signal bit on the left. As long as the value of the signal bit
  -- is not zero, continue reading.
  case x .&. fromInt 128 == zero of
    true ->
      let
        -- Apply bit mask (127 = %b01111111) for each individual byte
        -- that turns off the signal bit, so it couldn't create overlap
        -- issues when composing the target var-length number.
        uintArray = flip map (snoc acc x) $ and (fromInt 127)
        foldr_ m t f = foldr f m t
        -- Compose the target var-length number by folding the byte
        -- array and bitwise shifting lhs by 7 bit positions on each
        -- `foldr` pass so that the bytes overlap on the signal bit.
        uintValue = Tuple.fst
          $ foldr_ (zero /\ 0) uintArray
          $ \lhs (rhs /\ c) ->
              ((lhs `shl` fromInt (7 * c)) + rhs) /\ (c + 1)
      in
        Just $ wrap uintValue /\ xs
    _ -> fromVarLengthUInt xs (snoc acc x)
