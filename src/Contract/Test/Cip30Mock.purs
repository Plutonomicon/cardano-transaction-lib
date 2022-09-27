module Contract.Test.Cip30Mock
  ( module X
  ) where

import Ctl.Internal.Wallet.Cip30Mock
  ( WalletMock(MockFlint, MockGero, MockNami)
  , withCip30Mock
  ) as X
