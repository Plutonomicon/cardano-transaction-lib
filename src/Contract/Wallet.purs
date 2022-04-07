-- | A module with Wallet-related functionality.
module Contract.Wallet
  ( mkNamiWallet
  , module ContractAddress
  , module Wallet
  ) where

import Contract.Monad (Contract)
import Effect.Aff.Class (liftAff)
import Wallet
  ( NamiConnection
  , NamiWallet
  , Wallet(Nami)
  ) as Wallet
import Wallet (mkNamiWalletAff)
import Contract.Address
  ( getWalletAddress
  , getWalletCollateral
  ) as ContractAddress

-- | Make a wallet lifted into `Contract` from `Aff`.
mkNamiWallet :: forall (r :: Row Type). Contract r Wallet.Wallet
mkNamiWallet = liftAff mkNamiWalletAff