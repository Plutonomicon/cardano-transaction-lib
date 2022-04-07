module Plutus.IsoNativeForeign
  ( class IsoNativeForeign
  , toNativeType
  ) where

import Prelude
import Data.Array
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple), fst)
import Data.UInt (UInt, fromInt, toInt, (.&.), and, zshr, shl)
import Data.Newtype (class Newtype, wrap)
import Types.ByteArray (ByteArray, byteArrayToIntArray, byteArrayFromIntArray)
import Serialization.Hash (ed25519KeyHashFromBytes, scriptHashFromBytes) as Hash
import Serialization.Address (addressBytes)
import Serialization.Address (Address) as Foreign
import Plutus.Types.Address (Address) as Plutus
import Plutus.Types.Credential
  ( Credential(PubKeyCredential, ScriptCredential)
  , StakingCredential(StakingHash, StakingPtr)
  )

-- | This type class asserts that two types (native Purescript and
-- | foreign Javascript) are isomorphic, so they can be converted
-- | to each other without losing information.
class IsoNativeForeign :: Type -> Type -> Constraint
class IsoNativeForeign native frgn | native -> frgn, frgn -> native where
  toNativeType :: frgn -> Maybe native
-- TODO: toForeignType :: native -> frgn

--------------------------------------------------------------------------------
-- Isomorphic address types
--------------------------------------------------------------------------------

-- TODO: Currently only Shelley addresses are supported.
-- Should we introduce support for Byron and stake (reward) addresses as well?
instance IsoNativeForeign Plutus.Address Foreign.Address where
  -- Spec: https://github.com/cardano-foundation/CIPs/tree/master/CIP-0019
  -- | Attempts to build a Plutus-like address from a CSL-level address
  -- | represented by a sequence of bytes based on the CIP-0019
  -- | (specification describing the structure of addresses in Cardano).
  toNativeType addrForeign = addrType >>= \addrType' ->
    case toInt addrType' of -- TODO: Byron addresses
      -- %b0000 | network tag | key hash | key hash
      0 -> buildAddress pubKeyCredential $ Just $
        map StakingHash <<< pubKeyCredential

      -- %b0001 | network tag | script hash | key hash
      1 -> buildAddress scriptCredential $ Just $
        map StakingHash <<< pubKeyCredential

      -- %b0010 | network tag | key hash | script hash
      2 -> buildAddress pubKeyCredential $ Just $
        map StakingHash <<< scriptCredential

      -- %b0011 | network tag | script hash | script hash
      3 -> buildAddress scriptCredential $ Just $
        map StakingHash <<< scriptCredential

      -- %b0100 | network tag | key hash | pointer
      4 -> buildAddress pubKeyCredential $ Just stakingPtr

      -- %b0101 | network tag | script hash | pointer
      5 -> buildAddress scriptCredential $ Just stakingPtr

      -- %b0110 | network tag | key hash
      6 -> buildAddress pubKeyCredential Nothing

      -- %b0111 | network tag | script hash
      7 -> buildAddress scriptCredential Nothing

      _ -> Nothing
     where
     addrBytes :: Array Int
     addrBytes = byteArrayToIntArray $ addressBytes addrForeign

     -- | Retrieves the address type by reading
     -- | the first 4 bits (from the left) of the header-byte.
     addrType :: Maybe UInt
     addrType = head addrBytes >>= (pure <<< flip zshr (fromInt 4) <<< fromInt)

     pubKeyCredential :: ByteArray -> Maybe Credential
     pubKeyCredential =
       map (PubKeyCredential <<< wrap) <<< Hash.ed25519KeyHashFromBytes

     scriptCredential :: ByteArray -> Maybe Credential
     scriptCredential =
       map (ScriptCredential <<< wrap) <<< Hash.scriptHashFromBytes

     paymentPartHash :: Maybe ByteArray
     paymentPartHash = byteArrayFromIntArray $ take 28 (drop 1 addrBytes)

     delegationPartHash :: Maybe ByteArray
     delegationPartHash = byteArrayFromIntArray $ drop 29 addrBytes

     buildAddress
       :: (ByteArray -> Maybe Credential)
       -> Maybe (ByteArray -> Maybe StakingCredential)
       -> Maybe Plutus.Address
     buildAddress credential stakingCredential =
       paymentPartHash >>= credential >>= \c ->
         case stakingCredential of
           Nothing -> Just $
             wrap { addressCredential: c, addressStakingCredential: Nothing }
           Just stakingCredential ->
             delegationPartHash >>= stakingCredential >>= \sc -> Just $
               wrap { addressCredential: c, addressStakingCredential: Just sc }

     stakingPtr :: ByteArray -> Maybe StakingCredential
     stakingPtr byteArray = do
       Tuple slot bytes0 <- _parseChainPtr (byteArrayToIntArray byteArray) []
       Tuple txIx bytes1 <- _parseChainPtr bytes0 []
       Tuple certIx _ <- _parseChainPtr bytes1 []
       pure $ StakingPtr { slot, txIx, certIx }

-- | Extracts the variable-length positive number from a byte array
-- | following the ABNF grammar below:
-- |
-- | variable-length-uint =
-- |     (%b1 | uint7 | variable-length-uint)
-- |   / (%b0 | uint7)
-- | uint7 = 7bit
_parseChainPtr
  :: forall t
   . Newtype t UInt
  => Array Int
  -> Array UInt
  -> Maybe (Tuple t (Array Int))
_parseChainPtr bytes acc = do
  { head: x', tail: xs } <- uncons bytes
  let x = fromInt x'
  -- Applying bit mask (128 = %b10000000) for inspecting the
  -- signal bit on the left. As long as the value of the signal bit
  -- is not zero, continue reading.
  case (x .&. fromInt 128 == zero) of
    true ->
      let
        -- Applying bit mask (127 = %b01111111) for each individual byte
        -- that turns off the signal bit, so it couldn't create overlap
        -- issues when composing the target var-length number.
        uintArray = flip map (snoc acc x) $ and (fromInt 127)
        foldr_ m t f = foldr f m t
        -- Composing the target var-length number by folding the byte
        -- array and bitwise shifting lhs by 7 bit positions on each
        -- `foldr` pass so that the bytes overlap on the signal bit.
        uintValue = fst
          $ foldr_ (Tuple zero 0) uintArray
          $ \lhs (Tuple rhs c) ->
              Tuple ((lhs `shl` fromInt (7 * c)) + rhs) (c + 1)
      in
        Just $ Tuple (wrap uintValue) xs
    _ -> _parseChainPtr xs (snoc acc x)
