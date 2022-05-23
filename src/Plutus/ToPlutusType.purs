module Plutus.ToPlutusType
  ( class ToPlutusType
  , toPlutusType
  ) where

import Prelude

import Cardano.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  , UtxoM(UtxoM)
  ) as Cardano
import Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  ) as Cardano
import Cardano.Types.Value (Coin(Coin), Value(Value)) as Cardano
import Cardano.Types.Value
  ( NonAdaAsset(NonAdaAsset)
  , getCurrencySymbol
  )
import Data.Array (head, uncons, snoc, concatMap, take, drop, foldr)
import Data.Identity (Identity(Identity))
import Data.Foldable (fold)
import Data.Map (toUnfoldable) as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (traverse)
import Data.Tuple (fst) as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt, fromInt, (.&.), and, shl, zshr)
import Partial.Unsafe (unsafePartial)
import Plutus.Types.Address (Address) as Plutus
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
  )
import Plutus.Types.Credential
  ( Credential(PubKeyCredential, ScriptCredential)
  , StakingCredential(StakingHash, StakingPtr)
  )
import Plutus.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  , UtxoM(UtxoM)
  ) as Plutus
import Plutus.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  ) as Plutus
import Plutus.Types.Value (Coin, Value) as Plutus
import Plutus.Types.Value (lovelaceValueOf, singleton') as Plutus.Value
import Serialization.Address (Address) as Serialization
import Serialization.Address (addressBytes) as Serialization.Address
import Serialization.Hash (ed25519KeyHashFromBytes, scriptHashFromBytes)
import Types.ByteArray (ByteArray, byteArrayFromIntArray, byteArrayToIntArray)
import Types.TokenName (getTokenName)

class ToPlutusType :: (Type -> Type) -> Type -> Type -> Constraint
class ToPlutusType f t pt | t -> pt, t pt -> f where
  toPlutusType :: t -> f pt

--------------------------------------------------------------------------------
-- Cardano.Types.Value -> Plutus.Types.Value
--------------------------------------------------------------------------------

-- The underlying `Plutus.Types.AssocMap` of `Plutus.Types.Value` doesn't
-- have the `Ord` constraint on the keys. Therefore, one should be careful when
-- performing conversions between `Value`s, since the ordering of components
-- can't be guaranteed.
instance ToPlutusType Identity Cardano.Value Plutus.Value where
  toPlutusType
    (Cardano.Value (Cardano.Coin adaAmount) (NonAdaAsset nonAdaAssets)) =
    Identity (adaValue <> fold nonAdaValues)
    where
    adaValue :: Plutus.Value
    adaValue
      | adaAmount == zero = mempty
      | otherwise = Plutus.Value.lovelaceValueOf adaAmount

    nonAdaValues :: Array Plutus.Value
    nonAdaValues =
      flip concatMap (Map.toUnfoldable nonAdaAssets) $ \(cs /\ tokens) ->
        Map.toUnfoldable tokens <#> \(tn /\ val) ->
          unsafePartial $ fromJust $
            Plutus.Value.singleton' (getCurrencySymbol cs) (getTokenName tn) val

--------------------------------------------------------------------------------
-- Cardano.Types.Value.Coin -> Plutus.Types.Value.UtxoM
--------------------------------------------------------------------------------

instance ToPlutusType Identity Cardano.Coin Plutus.Coin where
  toPlutusType = pure <<< wrap <<< unwrap

--------------------------------------------------------------------------------
-- Serialization.Address -> Maybe Plutus.Types.Address
--------------------------------------------------------------------------------

instance ToPlutusType Maybe Serialization.Address Plutus.Address where
  -- | Attempts to build a Plutus address from a CSL-level address
  -- | represented by a sequence of bytes based on the CIP-0019.
  toPlutusType addrForeign =
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
    addrBytes = byteArrayToIntArray $
      Serialization.Address.addressBytes addrForeign

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

--------------------------------------------------------------------------------
-- Cardano.Types.Transaction.TransactionOutput ->
-- Maybe Plutus.Types.Transaction.TransactionOutput
--------------------------------------------------------------------------------

instance ToPlutusType Maybe Cardano.TransactionOutput Plutus.TransactionOutput
  where
  toPlutusType
    (Cardano.TransactionOutput { address, amount, dataHash }) = do
    addr <- toPlutusType address
    pure $ Plutus.TransactionOutput
      { address: addr, amount: unwrap $ toPlutusType amount, dataHash }

--------------------------------------------------------------------------------
-- Cardano.Types.Transaction.UtxoM -> Maybe Plutus.Types.Transaction.UtxoM
--------------------------------------------------------------------------------

instance ToPlutusType Maybe Cardano.UtxoM Plutus.UtxoM where
  toPlutusType (Cardano.UtxoM utxos) =
    Plutus.UtxoM <$> traverse toPlutusType utxos

--------------------------------------------------------------------------------
-- Cardano.Types.Transaction.TransactionUnspentOutput ->
-- Maybe Plutus.Types.Transaction.TransactionUnspentOutput
--------------------------------------------------------------------------------

instance
  ToPlutusType Maybe
    Cardano.TransactionUnspentOutput
    Plutus.TransactionUnspentOutput where
  toPlutusType (Cardano.TransactionUnspentOutput { input, output }) = do
    pOutput <- toPlutusType output
    pure $ Plutus.TransactionUnspentOutput { input, output: pOutput }
