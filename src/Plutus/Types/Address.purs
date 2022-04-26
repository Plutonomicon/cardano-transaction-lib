module Plutus.Types.Address
  ( Address(Address)
  , ForeignAddress(ForeignAddress)
  , pubKeyHashAddress
  , scriptHashAddress
  , toPubKeyHash
  , toValidatorHash
  , toStakingCredential
  ) where

import Prelude

import Data.Array (head, singleton, uncons, take, drop, foldr, snoc)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt, fromInt, toInt, (.&.), and, (.|.), zshr, shl)
import FromData (class FromData, fromData)
import ToData (class ToData, toData)
import Types.UnbalancedTransaction (PubKeyHash)
import Types.Scripts (ValidatorHash)
import Types.PlutusData (PlutusData(Constr))
import Plutus.ToFromPlutusType (class FromPlutusType, class ToPlutusType)
import Serialization.Address (Pointer, addressBytes, addressFromBytes)
import Serialization.Address (Address) as Foreign
import Types.ByteArray
  ( ByteArray
  , byteArrayFromIntArray
  , byteArrayFromIntArrayUnsafe
  , byteArrayToIntArray
  )
import Serialization.Hash
  ( ed25519KeyHashFromBytes
  , ed25519KeyHashToBytes
  , scriptHashFromBytes
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
  , addrHeaderType
  , addrHeaderTypeUInt
  )

--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------

-- Taken from https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Tx.html#t:Address
-- Plutus rev: dbefda30be6490c758aa88b600f5874f12712b3a
-- | Address with two kinds of credentials, normal and staking.
newtype Address = Address
  { addressCredential :: Credential
  , addressStakingCredential :: Maybe StakingCredential
  }

derive instance Eq Address
derive instance Ord Address
derive instance Newtype Address _
derive instance Generic Address _

instance Show Address where
  show = genericShow

instance ToData Address where
  toData (Address a) = Constr zero $
    [ toData a.addressCredential, toData a.addressStakingCredential ]

instance FromData Address where
  fromData (Constr n [ credD, stakingCredD ]) | n == zero =
    Address <$>
      ( { addressCredential: _, addressStakingCredential: _ }
          <$> fromData credD
          <*> fromData stakingCredD
      )
  fromData _ = Nothing

--------------------------------------------------------------------------------
-- Useful functions
--------------------------------------------------------------------------------

-- | The address that should be targeted by a transaction output locked
-- | by the public key with the given hash.
pubKeyHashAddress :: PubKeyHash -> Address
pubKeyHashAddress pkh = wrap $
  { addressCredential: PubKeyCredential pkh
  , addressStakingCredential: Nothing
  }

-- | The address that should be used by a transaction output locked
-- | by the given validator script hash.
scriptHashAddress :: ValidatorHash -> Address
scriptHashAddress vh = wrap $
  { addressCredential: ScriptCredential vh
  , addressStakingCredential: Nothing
  }

-- | The PubKeyHash of the address (if any).
toPubKeyHash :: Address -> Maybe PubKeyHash
toPubKeyHash addr =
  case _.addressCredential (unwrap addr) of
    PubKeyCredential k -> Just k
    _ -> Nothing

-- | The validator hash of the address (if any).
toValidatorHash :: Address -> Maybe ValidatorHash
toValidatorHash addr =
  case _.addressCredential (unwrap addr) of
    ScriptCredential k -> Just k
    _ -> Nothing

-- | The staking credential of an address (if any).
toStakingCredential :: Address -> Maybe StakingCredential
toStakingCredential = _.addressStakingCredential <<< unwrap

--------------------------------------------------------------------------------
-- FromPlutusType / ToPlutusType
--------------------------------------------------------------------------------

newtype ForeignAddress = ForeignAddress Foreign.Address

derive instance Newtype ForeignAddress _

instance FromPlutusType Maybe Address ForeignAddress where
  -- | Attempts to build a CSL-level address from a Plutus address
  -- | based on the CIP-0019.
  fromPlutusType addrPlutus = ForeignAddress <$>
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

instance ToPlutusType Maybe ForeignAddress Address where
  -- | Attempts to build a Plutus address from a CSL-level address
  -- | represented by a sequence of bytes based on the CIP-0019.
  toPlutusType (ForeignAddress addrForeign) =
    addrType >>= addrHeaderType >>= \addrType' ->
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
      -> Maybe Address
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
      slot /\ bytes0 <- _fromVarLengthUInt (byteArrayToIntArray byteArray) []
      txIx /\ bytes1 <- _fromVarLengthUInt bytes0 []
      certIx /\ _ <- _fromVarLengthUInt bytes1 []
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
  -> Maybe (t /\ Array Int)
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
          $ foldr_ (zero /\ 0) uintArray
          $ \lhs (rhs /\ c) ->
              ((lhs `shl` fromInt (7 * c)) + rhs) /\ (c + 1)
      in
        Just $ wrap uintValue /\ xs
    _ -> _fromVarLengthUInt xs (snoc acc x)
