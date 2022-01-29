module Wallet
  ( NamiWallet
  , Wallet(..)
  , mockNamiWallet
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Typelevel.Undefined (undefined)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Types.Transaction (Address, TransactionOutput)

-- At the moment, we only support Nami's wallet. In the future we will expand
-- this with more constructors to represent out-of-browser wallets (e.g. WBE)
data Wallet (w :: Type) = Nami (NamiWallet w)

-------------------------------------------------------------------------------
-- Nami backend
-------------------------------------------------------------------------------
-- Record-of-functions for real or mocked Nami wallet, includes `Ref` to
-- connection (e.g. with `window.cardano` as a `NamiConnection`)
type NamiWallet (w :: Type) =
  { connection :: Ref.Ref w
  -- ^ A reference to a connection with Nami, i.e. `window.cardano.nami`
  , enable :: w -> Aff w
  -- ^ Request that the user grant access to their nami wallet
  , getWalletAddress :: w -> Aff Address
  -- ^ Get the address associated with the wallet (Nami does not support
  -- multiple addresses)
  , getCollateral :: w -> Aff (Maybe TransactionOutput)
  -- ^ Get the collateral UTxO associated with the Nami wallet
  }

-- A Nami wallet with all functions mocked
mockNamiWallet :: Aff (Wallet Unit)
mockNamiWallet =
  liftEffect
    $ do
        connection <- Ref.new unit
        pure
          $ Nami
              { connection
              , enable: const $ pure unit
              , getWalletAddress: undefined -- TODO
              , getCollateral: const $ pure Nothing
              }
