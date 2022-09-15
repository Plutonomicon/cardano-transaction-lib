-- | A module for mocking CIP30 wallets.
module CTL.Contract.Wallet.Cip30Mock (module X) where

import CTL.Internal.Wallet.Cip30Mock
  ( WalletMock(MockFlint, MockGero, MockNami)
  , withCip30Mock
  ) as X
