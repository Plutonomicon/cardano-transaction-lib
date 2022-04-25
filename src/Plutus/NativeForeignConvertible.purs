module Plutus.NativeForeignConvertible
  ( class NativeForeignConvertible
  , toNativeType
  , toForeignType
  ) where

import Prelude
import Data.Array (head, uncons, take, drop, foldr, singleton, snoc)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple), fst)
import Data.UInt (UInt, fromInt, toInt, (.&.), and, (.|.), zshr, shl)
import Data.Newtype (class Newtype, wrap, unwrap)
import Serialization.Address (Pointer, addressBytes, addressFromBytes)
import Serialization.Address (Address) as Foreign
import Plutus.Types.Address (Address) as Plutus
import Types.ByteArray
  ( ByteArray
  , byteArrayToIntArray
  , byteArrayFromIntArray
  , byteArrayFromIntArrayUnsafe
  )
import Serialization.Hash
  ( ed25519KeyHashFromBytes
  , scriptHashFromBytes
  , ed25519KeyHashToBytes
  , scriptHashToBytes
  )
import Plutus.Types.Credential
  ( Credential(PubKeyCredential, ScriptCredential)
  , StakingCredential(StakingHash, StakingPtr)
  )
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
  , addrHeaderTypeUInt
  , addrHeaderType
  )

-- | This type class asserts that two types (native Purescript and
-- | foreign Javascript) can be converted to each other.
class
  NativeForeignConvertible (native :: Type) (frgn :: Type)
  | native -> frgn
  , frgn -> native where
  toNativeType :: frgn -> Maybe native
  toForeignType :: native -> Maybe frgn

--------------------------------------------------------------------------------
-- Isomorphic address types
--------------------------------------------------------------------------------

-- TODO: Currently only Shelley addresses are supported.
-- Should we introduce support for Byron and stake (reward) addresses as well?
--
-- https://github.com/cardano-foundation/CIPs/tree/master/CIP-0019
-- (specification describing the structure of addresses in Cardano)
instance NativeForeignConvertible Plutus.Address Foreign.Address where
  -- | Attempts to build a CSL-level address from a Plutus address
  -- | based on the CIP-0019.
  toForeignType addrPlutus =
    case rec.addressCredential, rec.addressStakingCredential of
      -- %b0000 | network tag | key hash | key hash
      PubKeyCredential pkh, Just (StakingHash (PubKeyCredential skh)) ->
        addressFromBytes $
          addrHeader PaymentKeyHashStakeKeyHash
            <> ed25519KeyHashToBytes (unwrap pkh)
            <> ed25519KeyHashToBytes (unwrap skh)

      -- %b0001 | network tag | script hash | key hash
      ScriptCredential sh, Just (StakingHash (PubKeyCredential skh)) ->
        addressFromBytes $
          addrHeader ScriptHashStakeKeyHash
            <> scriptHashToBytes (unwrap sh)
            <> ed25519KeyHashToBytes (unwrap skh)

      -- %b0010 | network tag | key hash | script hash
      PubKeyCredential pkh, Just (StakingHash (ScriptCredential sh)) ->
        addressFromBytes $
          addrHeader PaymentKeyHashScriptHash
            <> ed25519KeyHashToBytes (unwrap pkh)
            <> scriptHashToBytes (unwrap sh)

      -- %b0011 | network tag | script hash | script hash
      ScriptCredential sh, Just (StakingHash (ScriptCredential sh')) ->
        addressFromBytes $
          addrHeader ScriptHashScriptHash
            <> scriptHashToBytes (unwrap sh)
            <> scriptHashToBytes (unwrap sh')

      -- %b0100 | network tag | key hash | pointer
      PubKeyCredential pkh, Just (StakingPtr ptr) ->
        addressFromBytes $
          addrHeader PaymentKeyHashPointer
            <> ed25519KeyHashToBytes (unwrap pkh)
            <> pointerToBytes ptr

      -- %b0101 | network tag | script hash | pointer
      ScriptCredential sh, Just (StakingPtr ptr) ->
        addressFromBytes $
          addrHeader ScriptHashPointer
            <> scriptHashToBytes (unwrap sh)
            <> pointerToBytes ptr

      -- %b0110 | network tag | key hash
      PubKeyCredential pkh, Nothing ->
        addressFromBytes $
          addrHeader PaymentKeyHash
            <> ed25519KeyHashToBytes (unwrap pkh)

      -- %b0111 | network tag | script hash
      ScriptCredential sh, Nothing ->
        addressFromBytes $
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

    pointerToBytes :: Pointer -> ByteArray
    pointerToBytes ptr =
      _toVarLengthUInt ptr.slot <> _toVarLengthUInt ptr.txIx <>
        _toVarLengthUInt ptr.certIx

  -- | Attempts to build a Plutus address from a CSL-level address
  -- | represented by a sequence of bytes based on the CIP-0019.
  toNativeType addrForeign = addrType >>= addrHeaderType >>= case _ of
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
    addrBytes = byteArrayToIntArray $ addressBytes addrForeign

    -- | Retrieves the address type by reading
    -- | the first 4 bits (from the left) of the header-byte.
    addrType :: Maybe UInt
    addrType = head addrBytes >>= (pure <<< flip zshr (fromInt 4) <<< fromInt)

    -- | Retrieves the payment part of the address by reading
    -- | the first 28 bytes following the address header.
    paymentPartHash :: Maybe ByteArray
    paymentPartHash = byteArrayFromIntArray $ take 28 (drop 1 addrBytes)

    -- | Retrieves the delegation part of the address by reading
    -- | the bytes following the payment part.
    delegationPartHash :: Maybe ByteArray
    delegationPartHash = byteArrayFromIntArray $ drop 29 addrBytes

    pubKeyCredential :: ByteArray -> Maybe Credential
    pubKeyCredential =
      map (PubKeyCredential <<< wrap) <<< ed25519KeyHashFromBytes

    scriptCredential :: ByteArray -> Maybe Credential
    scriptCredential =
      map (ScriptCredential <<< wrap) <<< scriptHashFromBytes

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
      Tuple slot bytes0 <- _fromVarLengthUInt (byteArrayToIntArray byteArray) []
      Tuple txIx bytes1 <- _fromVarLengthUInt bytes0 []
      Tuple certIx _ <- _fromVarLengthUInt bytes1 []
      pure $ StakingPtr { slot, txIx, certIx }

-- | Extracts the variable-length positive number from a byte array
-- | according to the ABNF grammar below:
-- |
-- | variable-length-uint =
-- |     (%b1 | uint7 | variable-length-uint)
-- |   / (%b0 | uint7)
-- | uint7 = 7bit
_fromVarLengthUInt
  :: forall (t :: Type)
   . Newtype t UInt
  => Array Int
  -> Array UInt
  -> Maybe (Tuple t (Array Int))
_fromVarLengthUInt bytes acc = do
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
        uintValue = fst
          $ foldr_ (Tuple zero 0) uintArray
          $ \lhs (Tuple rhs c) ->
              Tuple ((lhs `shl` fromInt (7 * c)) + rhs) (c + 1)
      in
        Just $ Tuple (wrap uintValue) xs
    _ -> _fromVarLengthUInt xs (snoc acc x)

-- | Encodes the variable-length positive number as a byte array
-- | according to the ABNF grammar below.
-- |
-- | variable-length-uint =
-- |     (%b1 | uint7 | variable-length-uint)
-- |   / (%b0 | uint7)
-- | uint7 = 7bit
_toVarLengthUInt :: forall (t :: Type). Newtype t UInt => t -> ByteArray
_toVarLengthUInt t = worker (unwrap t) false
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
