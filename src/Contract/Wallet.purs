-- | A module with Wallet-related functionality.
module Contract.Wallet
  ( mkKeyWalletFromPrivateKeys
  , withKeyWallet
  , withKeyWalletFromMnemonic
  , ownStakePubKeyHash
  , ownPaymentPubKeyHash
  , getWalletBalance
  , getWalletUtxos
  , getWalletCollateral
  , getWalletAddress
  , getWalletAddresses
  , getWalletAddressWithNetworkTag
  , getWalletAddressesWithNetworkTag
  , module X
  ) where

import Prelude

import Contract.Address
  ( Address
  , AddressWithNetworkTag
  , PaymentPubKeyHash
  , StakePubKeyHash
  )
import Contract.Config (PrivatePaymentKey, PrivateStakeKey)
import Contract.Log (logTrace')
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.Sync (syncBackendWithWallet, withoutSync)
import Contract.Transaction (TransactionUnspentOutput)
import Contract.Utxos (UtxoMap)
import Contract.Value (Value)
import Contract.Value as Value
import Contract.Wallet.Key (KeyWallet, privateKeysToKeyWallet)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Reader (asks, local)
import Ctl.Internal.Contract.Wallet
  ( getChangeAddress
  , getRewardAddresses
  , getUnusedAddresses
  , getWallet
  , ownPaymentPubKeyHashes
  , ownStakePubKeyHashes
  , signData
  ) as X
import Ctl.Internal.Contract.Wallet (getWalletUtxos) as Wallet
import Ctl.Internal.Contract.Wallet
  ( ownPaymentPubKeyHashes
  , ownStakePubKeyHashes
  )
import Ctl.Internal.Contract.Wallet as Contract
import Ctl.Internal.Deserialization.Keys (privateKeyFromBytes) as X
import Ctl.Internal.Helpers (liftM)
import Ctl.Internal.Plutus.Conversion
  ( toPlutusAddress
  , toPlutusTxUnspentOutput
  , toPlutusUtxoMap
  )
import Ctl.Internal.Plutus.Conversion.Address (toPlutusAddressWithNetworkTag)
import Ctl.Internal.Wallet (Wallet(KeyWallet)) as Wallet
import Ctl.Internal.Wallet
  ( Wallet(KeyWallet, GenericCip30)
  , WalletExtension
      ( NamiWallet
      , GeroWallet
      , FlintWallet
      , EternlWallet
      , LodeWallet
      , LaceWallet
      , NuFiWallet
      , GenericCip30Wallet
      )
  , isWalletAvailable
  ) as X
import Ctl.Internal.Wallet.Key
  ( KeyWallet
  , PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  , privateKeysToKeyWallet
  ) as X
import Ctl.Internal.Wallet.KeyFile (formatPaymentKey, formatStakeKey) as X
import Ctl.Internal.Wallet.Spec
  ( Cip1852DerivationPath
  , StakeKeyPresence
  , mkKeyWalletFromMnemonic
  )
import Ctl.Internal.Wallet.Spec
  ( MnemonicSource(MnemonicString, MnemonicFile)
  , PrivatePaymentKeySource(PrivatePaymentKeyFile, PrivatePaymentKeyValue)
  , PrivateStakeKeySource(PrivateStakeKeyFile, PrivateStakeKeyValue)
  , WalletSpec
      ( UseKeys
      , UseMnemonic
      , ConnectToNami
      , ConnectToGero
      , ConnectToFlint
      , ConnectToLode
      , ConnectToLace
      , ConnectToEternl
      , ConnectToNuFi
      , ConnectToGenericCip30
      )
  ) as X
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Foldable (fold, foldr)
import Data.Map as Map
import Data.Maybe (Maybe(Just), fromMaybe)
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Data.Tuple.Nested ((/\))
import Effect.Exception (error)
import Prim.TypeError (class Warn, Text)

withKeyWallet
  :: forall (a :: Type)
   . KeyWallet
  -> Contract a
  -> Contract a
withKeyWallet wallet =
  local _ { wallet = Just $ Wallet.KeyWallet wallet }

withKeyWalletFromMnemonic
  :: forall (a :: Type)
   . String
  -> Cip1852DerivationPath
  -> StakeKeyPresence
  -> Contract a
  -> Contract a
withKeyWalletFromMnemonic mnemonic derivationPath stakeKeyPresence contract = do
  keyWallet <- liftEither $ lmap (error <<< addNote) $
    mkKeyWalletFromMnemonic mnemonic derivationPath stakeKeyPresence
  withKeyWallet keyWallet contract
  where
  addNote = append "withKeyWalletFromMnemonic: "

mkKeyWalletFromPrivateKeys
  :: PrivatePaymentKey -> Maybe PrivateStakeKey -> KeyWallet
mkKeyWalletFromPrivateKeys payment mbStake = privateKeysToKeyWallet payment
  mbStake

-- | Get an `Address` of the browser wallet.
getWalletAddress
  :: Warn
       ( Text
           "This function returns only one `Adress` even in case multiple `Adress`es are available. Use `getWalletAdresses` instead"
       )
  => Contract (Maybe Address)
getWalletAddress = head <$> getWalletAddresses

-- | Get all the `Address`es of the browser wallet.
getWalletAddresses :: Contract (Array Address)
getWalletAddresses = do
  addresses <- Contract.getWalletAddresses
  traverse
    ( liftM
        (error "getWalletAddresses: failed to deserialize `Address`")
        <<< toPlutusAddress
    )
    addresses

-- | Get an `AddressWithNetworkTag` of the browser wallet.
getWalletAddressWithNetworkTag
  :: Warn
       ( Text
           "This function returns only one `AddressWithNetworkTag` even in case multiple `AddressWithNetworkTag` are available. Use `getWalletAddressesWithNetworkTag` instead"
       )
  => Contract (Maybe AddressWithNetworkTag)
getWalletAddressWithNetworkTag = head <$> getWalletAddressesWithNetworkTag

-- | Get all the `AddressWithNetworkTag`s of the browser wallet discarding errors.
getWalletAddressesWithNetworkTag :: Contract (Array AddressWithNetworkTag)
getWalletAddressesWithNetworkTag = do
  addresses <- Contract.getWalletAddresses
  traverse
    ( liftM
        ( error
            "getWalletAddressesWithNetworkTag: failed to deserialize `Address`"
        )
        <<< toPlutusAddressWithNetworkTag
    )
    addresses

-- | Gets a wallet `PaymentPubKeyHash` via `getWalletAddresses`.
ownPaymentPubKeyHash
  :: Warn
       ( Text
           "This function returns only one `PaymentPubKeyHash` even in case multiple `PaymentPubKeysHash`es are available. Use `ownPaymentPubKeyHashes` instead"
       )
  => Contract (Maybe PaymentPubKeyHash)
ownPaymentPubKeyHash = head <$> ownPaymentPubKeyHashes

ownStakePubKeyHash
  :: Warn
       ( Text
           "This function returns only one `StakePubKeyHash` even in case multiple `StakePubKeysHash`es are available. Use `ownStakePubKeyHashes` instead"
       )
  => Contract (Maybe StakePubKeyHash)
ownStakePubKeyHash = join <<< head <$> ownStakePubKeyHashes

-- | Similar to `utxosAt` called on own address, except that it uses CIP-30
-- | wallet state and not query layer state.
-- | The user should not expect these states to be in sync.
-- | When active wallet is `KeyWallet`, query layer state is used.
-- | This function is expected to be more performant than `utxosAt` when there
-- | is a large number of assets.
getWalletUtxos
  :: Contract (Maybe UtxoMap)
getWalletUtxos = do
  logTrace' "getWalletUtxos"
  whenM
    ( asks $
        _.synchronizationParams
          >>> _.syncBackendWithWallet
          >>> _.beforeCip30Methods
    )
    syncBackendWithWallet
  mCardanoUtxos <- Wallet.getWalletUtxos
  for mCardanoUtxos $
    liftContractM "getWalletUtxos: unable to deserialize UTxOs" <<<
      toPlutusUtxoMap

getWalletBalance
  :: Contract (Maybe Value)
getWalletBalance = do
  logTrace' "getWalletBalance"
  whenM
    ( asks $ _.synchronizationParams
        >>> _.syncBackendWithWallet
        >>> _.beforeCip30Methods
    )
    syncBackendWithWallet
  let
    getUtxoValue = unwrap >>> _.output >>> unwrap >>> _.amount
    sumValues = foldr (Value.unionWith add) mempty
  -- include both spendable UTxOs and collateral
  utxos <- getWalletUtxos <#> fromMaybe Map.empty
  collateralUtxos <- withoutSync getWalletCollateral <#> fold >>> toUtxoMap
  let allUtxos = Map.union utxos collateralUtxos
  pure $ pure $ sumValues $ map getUtxoValue $ Map.values allUtxos
  where
  toUtxoMap = Map.fromFoldable <<< map
    (unwrap >>> \({ input, output }) -> input /\ output)

-- | Get the collateral of the browser wallet. This collateral will vary
-- | depending on the wallet.
-- | E.g. Nami creates a hard-coded 5 Ada collateral.
-- | Throws on `Promise` rejection by wallet, returns `Nothing` if no collateral
-- | is available.
getWalletCollateral
  :: Contract (Maybe (Array TransactionUnspentOutput))
getWalletCollateral = do
  logTrace' "getWalletCollateral"
  whenM
    ( asks
        ( _.synchronizationParams
            >>> _.syncBackendWithWallet
            >>> _.beforeCip30Methods
        )
    )
    syncBackendWithWallet
  mtxUnspentOutput <- Contract.getWalletCollateral
  for mtxUnspentOutput $ traverse $
    liftedM
      "getWalletCollateral: failed to deserialize TransactionUnspentOutput"
      <<< pure
      <<< toPlutusTxUnspentOutput
