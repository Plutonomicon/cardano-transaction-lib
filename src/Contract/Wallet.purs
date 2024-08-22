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
  , module X
  ) where

import Prelude

import Cardano.Types (Address, StakePubKeyHash, UtxoMap, Value)
import Cardano.Types.PaymentPubKeyHash (PaymentPubKeyHash)
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Cardano.Types.Value as Value
import Cardano.Wallet.Key
  ( KeyWallet
  , PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  , privateKeysToKeyWallet
  ) as X
import Cardano.Wallet.Key (PrivateDrepKey)
import Contract.Config (PrivatePaymentKey, PrivateStakeKey)
import Contract.Log (logTrace')
import Contract.Monad (Contract)
import Contract.Sync (syncBackendWithWallet, withoutSync)
import Contract.Wallet.Key (KeyWallet, privateKeysToKeyWallet)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Reader (asks, local)
import Ctl.Internal.Contract.Wallet
  ( getChangeAddress
  , getRewardAddresses
  , getUnusedAddresses
  , getWallet
  , getWalletAddresses
  , ownDrepPubKey
  , ownDrepPubKeyHash
  , ownPaymentPubKeyHashes
  , ownRegisteredPubStakeKeys
  , ownStakePubKeyHashes
  , ownUnregisteredPubStakeKeys
  , signData
  ) as X
import Ctl.Internal.Contract.Wallet
  ( getWalletAddresses
  , ownPaymentPubKeyHashes
  , ownStakePubKeyHashes
  )
import Ctl.Internal.Contract.Wallet (getWalletUtxos) as Wallet
import Ctl.Internal.Contract.Wallet as Contract
import Ctl.Internal.Helpers (liftM)
import Ctl.Internal.Wallet
  ( Cip30Extensions
  , Wallet(KeyWallet, GenericCip30)
  , WalletExtension
  , isWalletAvailable
  ) as X
import Ctl.Internal.Wallet (Wallet(KeyWallet)) as Wallet
import Ctl.Internal.Wallet.KeyFile (formatPaymentKey, formatStakeKey) as X
import Ctl.Internal.Wallet.Spec
  ( Cip1852DerivationPath
  , StakeKeyPresence
  , mkKeyWalletFromMnemonic
  )
import Ctl.Internal.Wallet.Spec
  ( KnownWallet(Nami, Gero, Flint, Eternl, Lode, Lace, NuFi)
  , MnemonicSource(MnemonicString, MnemonicFile)
  , PrivatePaymentKeySource(PrivatePaymentKeyFile, PrivatePaymentKeyValue)
  , PrivateStakeKeySource(PrivateStakeKeyFile, PrivateStakeKeyValue)
  , WalletSpec(UseKeys, UseMnemonic, ConnectToGenericCip30)
  , walletName
  ) as X
import Data.Array (head)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Foldable (fold)
import Data.Map as Map
import Data.Maybe (Maybe(Just), fromMaybe)
import Data.Newtype (unwrap)
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
  :: PrivatePaymentKey
  -> Maybe PrivateStakeKey
  -> Maybe PrivateDrepKey
  -> KeyWallet
mkKeyWalletFromPrivateKeys payment mbStake = privateKeysToKeyWallet payment
  mbStake

-- | Get the first `Address` of the wallet.
getWalletAddress
  :: Warn
       ( Text
           "This function returns only one `Address` even in case multiple `Address`es are available. Use `getWalletAddresses` instead"
       )
  => Contract (Maybe Address)
getWalletAddress = head <$> getWalletAddresses

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
  Wallet.getWalletUtxos

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
    getUtxoValue = unwrap >>> _.amount
  -- include both spendable UTxOs and collateral
  utxos <- getWalletUtxos <#> fromMaybe Map.empty
  collateralUtxos <- withoutSync getWalletCollateral <#> fold >>> toUtxoMap
  let allUtxos = Map.union utxos collateralUtxos
  let
    mbValueSum = Value.sum $ map getUtxoValue $ Array.fromFoldable $ Map.values
      allUtxos
  pure <$> liftM (error "getWalletBalance: overflow when summing `Value`s")
    mbValueSum
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
  Contract.getWalletCollateral
