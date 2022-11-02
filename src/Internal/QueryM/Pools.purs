module Ctl.Internal.QueryM.Pools
  ( getPoolIds
  , getPoolParameters
  , parseIpv6String
  , getPubKeyHashDelegationsAndRewards
  , getValidatorHashDelegationsAndRewards
  , getDelegationsAndRewards
  , DelegationsAndRewards
  ) where

import Prelude

import Aeson (Aeson, JsonDecodeError(TypeMismatch), decodeAeson, (.:), (.:?))
import Control.Alt ((<|>))
import Control.Alternative (guard)
import Ctl.Internal.Cardano.Types.Transaction
  ( Ipv4(Ipv4)
  , Ipv6(Ipv6)
  , PoolMetadata(PoolMetadata)
  , PoolMetadataHash(PoolMetadataHash)
  , PoolPubKeyHash
  , PoolRegistrationParams
  , Relay(MultiHostName, SingleHostName, SingleHostAddr)
  , URL(URL)
  , UnitInterval
  )
import Ctl.Internal.Cardano.Types.Value (Coin(Coin))
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.Helpers (liftEither)
import Ctl.Internal.Plutus.Types.Credential
  ( Credential(PubKeyCredential, ScriptCredential)
  )
import Ctl.Internal.QueryM (QueryM, mkOgmiosRequest)
import Ctl.Internal.QueryM.Ogmios as Ogmios
import Ctl.Internal.Serialization.Hash
  ( ed25519KeyHashToBech32
  , ed25519KeyHashToBech32Unsafe
  , ed25519KeyHashToBytes
  , scriptHashToBech32Unsafe
  , scriptHashToBytes
  )
import Ctl.Internal.Types.BigNum as BigNum
import Ctl.Internal.Types.ByteArray
  ( byteArrayFromIntArray
  , byteArrayToHex
  , hexToByteArray
  )
import Ctl.Internal.Types.PubKeyHash (StakePubKeyHash)
import Ctl.Internal.Types.Scripts (StakeValidatorHash)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(Right, Left), note)
import Data.Foldable (fold)
import Data.Int as Int
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)
import Data.String (Pattern(Pattern), Replacement(Replacement))
import Data.String as String
import Data.String.Utils as StringUtils
import Data.Traversable (for, traverse)
import Effect.Exception (error)
import Foreign.Object (Object)

getPoolIds :: QueryM (Array PoolPubKeyHash)
getPoolIds = mkOgmiosRequest Ogmios.queryPoolIdsCall
  _.poolIds
  unit

decodeUnitInterval :: Aeson -> Either JsonDecodeError UnitInterval
decodeUnitInterval aeson = do
  str <- decodeAeson aeson
  case String.split (Pattern "/") str of
    [ num, den ] -> do
      numerator <- note (TypeMismatch "BigNum") $ BigNum.fromString num
      denominator <- note (TypeMismatch "BigNum") $ BigNum.fromString den
      pure
        { numerator
        , denominator
        }
    _ -> Left $ TypeMismatch "UnitInterval"

decodeIpv4 :: Aeson -> Either JsonDecodeError Ipv4
decodeIpv4 aeson = do
  str <- decodeAeson aeson
  case String.split (Pattern ".") str of
    bs@[ _, _, _, _ ] -> do
      ints <- for bs $
        note (TypeMismatch "Ipv4") <<< Int.fromString
      Ipv4 <$> note (TypeMismatch "Ipv4") (byteArrayFromIntArray ints)
    _ -> Left $ TypeMismatch "Ipv4"

decodeIpv6 :: Aeson -> Either JsonDecodeError Ipv6
decodeIpv6 aeson = do
  decodeAeson aeson >>= parseIpv6String >>> note (TypeMismatch "Ipv6")

parseIpv6String :: String -> Maybe Ipv6
parseIpv6String str = do
  let
    parts = String.split (Pattern ":") str
    partsFixed =
      if Array.length parts < 8 then
        -- Normalize double colon
        -- see https://ipcisco.com/lesson/ipv6-address/
        do
          part <- parts
          if part == "" then
            Array.replicate (8 - Array.length parts + 1) ""
          else
            pure part
      else
        parts
  guard (Array.length partsFixed == 8)
  let
    padded = String.replaceAll (Pattern " ") (Replacement "0") $ fold $
      partsFixed
        <#> StringUtils.padStart 4
  Ipv6 <$> hexToByteArray padded

decodeRelay :: Aeson -> Either JsonDecodeError Relay
decodeRelay aeson = do
  obj <- decodeAeson aeson
  let
    decodeSingleHostAddr = do
      port <- obj .:? "port"
      ipv4 <- obj .:? "ipv4" >>= traverse decodeIpv4
      ipv6 <- obj .:? "ipv6" >>= traverse decodeIpv6
      pure $ SingleHostAddr { port, ipv4, ipv6 }
    decodeSingleHostName = do
      port <- obj .: "port"
      dnsName <- obj .: "hostname"
      pure $ SingleHostName { port, dnsName }
    decodeMultiHostName = do
      dnsName <- obj .: "hostname"
      pure $ MultiHostName { dnsName }
  decodeSingleHostName <|> decodeSingleHostAddr <|> decodeMultiHostName

decodePoolMetadata :: Aeson -> Either JsonDecodeError PoolMetadata
decodePoolMetadata aeson = do
  obj <- decodeAeson aeson
  hash <- obj .: "hash" >>= note (TypeMismatch "PoolMetadataHash")
    <<< map PoolMetadataHash
    <<< hexToByteArray
  url <- obj .: "url" <#> URL
  pure $ PoolMetadata { hash, url }

-- TODO: batched variant
getPoolParameters :: PoolPubKeyHash -> QueryM PoolRegistrationParams
getPoolParameters poolPubKeyHash = do
  aeson <- mkOgmiosRequest Ogmios.queryPoolParameters
    _.poolParameters
    [ poolPubKeyHash ]
  let
    (params :: Either JsonDecodeError PoolRegistrationParams) = do
      obj <- decodeAeson aeson
      poolIdStr <- liftEither $ note (TypeMismatch "PoolPubKeyHash")
        $ ed25519KeyHashToBech32 "pool"
        $ unwrap poolPubKeyHash
      objParams :: Object Aeson <- obj .: poolIdStr
      vrfKeyhashHex <- objParams .: "vrf"
      vrfKeyhashBytes <- note (TypeMismatch "VRFKeyHash") $ hexToByteArray
        vrfKeyhashHex
      vrfKeyhash <- note (TypeMismatch "VRFKeyHash") $ fromBytes vrfKeyhashBytes
      pledge <- objParams .: "pledge"
      cost <- objParams .: "cost"
      margin <- decodeUnitInterval =<< objParams .: "margin"
      rewardAccount <- objParams .: "rewardAccount"
      poolOwners <- objParams .: "owners"
      relayArr <- objParams .: "relays"
      relays <- for relayArr decodeRelay
      poolMetadata <- objParams .:? "metadata" >>= traverse decodePoolMetadata
      pure
        { operator: poolPubKeyHash
        , vrfKeyhash
        , pledge
        , cost
        , margin
        , rewardAccount
        , poolOwners
        , relays
        , poolMetadata
        }
  liftEither $ lmap (error <<< show) params

type DelegationsAndRewards =
  { rewards :: Maybe Coin
  , delegate :: Maybe PoolPubKeyHash
  }

getDelegationsAndRewards :: Credential -> QueryM (Maybe DelegationsAndRewards)
getDelegationsAndRewards (PubKeyCredential pubKey) =
  getPubKeyHashDelegationsAndRewards $ wrap pubKey
getDelegationsAndRewards (ScriptCredential script) =
  getValidatorHashDelegationsAndRewards $ wrap $ unwrap script

getValidatorHashDelegationsAndRewards
  :: StakeValidatorHash -> QueryM (Maybe DelegationsAndRewards)
getValidatorHashDelegationsAndRewards skh = do
  aeson <- mkOgmiosRequest Ogmios.queryDelegationsAndRewards
    _.delegationsAndRewards
    [ stringRep
    ]
  let
    result = do
      obj <- decodeAeson aeson
      decodeAeson =<< obj .: byteHex
  case result of
    Left _ -> pure Nothing
    Right obj -> Just <$> do
      liftEither $ lmap (error <<< show) do
        rewards <- map Coin <$> obj .:? "rewards"
        delegate <- obj .:? "delegate"
        pure { rewards, delegate }
  where
  stringRep :: String
  stringRep = scriptHashToBech32Unsafe "script" $ unwrap skh

  byteHex :: String
  byteHex =
    byteArrayToHex <<< unwrap <<< scriptHashToBytes <<< unwrap $
      skh

-- TODO: batched variant
getPubKeyHashDelegationsAndRewards
  :: StakePubKeyHash -> QueryM (Maybe DelegationsAndRewards)
getPubKeyHashDelegationsAndRewards pkh = do
  aeson <- mkOgmiosRequest Ogmios.queryDelegationsAndRewards
    _.delegationsAndRewards
    [ stringRep ]
  let
    result = do
      obj <- decodeAeson aeson
      decodeAeson =<< obj .: byteHex
  case result of
    Left _ -> pure Nothing
    Right obj -> Just <$> do
      liftEither $ lmap (error <<< show) do
        rewards <- map Coin <$> obj .:? "rewards"
        delegate <- obj .:? "delegate"
        pure { rewards, delegate }
  where
  stringRep :: String
  stringRep =
    ed25519KeyHashToBech32Unsafe "stake_vkh" $ unwrap $ unwrap pkh

  byteHex :: String
  byteHex =
    byteArrayToHex <<< unwrap <<< ed25519KeyHashToBytes <<< unwrap $
      unwrap
        pkh
