module Ctl.Internal.BalanceTx.FakeOutput
  ( fakeOutputWithValue
  , fakeOutputWithMultiAssets
  ) where

import Prelude

import Cardano.Types.Address as Address
import Cardano.Types.Coin as Coin
import Cardano.Types.MultiAsset (MultiAsset)
import Cardano.Types.TransactionOutput (TransactionOutput(TransactionOutput))
import Cardano.Types.Value (Value, mkValue)
import Data.Maybe (Maybe(Nothing), fromJust)
import Partial.Unsafe (unsafePartial)

fakeOutputWithValue :: Value -> TransactionOutput
fakeOutputWithValue amount =
  TransactionOutput
    { -- this fake address is taken from CSL:
      -- https://github.com/Emurgo/cardano-serialization-lib/blob/a58bfa583297705ffc0fb03923cecef3452a6aee/rust/src/utils.rs#L1146
      address: unsafePartial fromJust $ Address.fromBech32
        "addr_test1qpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5ewvxwdrt70qlcpe\
        \eagscasafhffqsxy36t90ldv06wqrk2qum8x5w"
    , amount
    , datum: Nothing
    , scriptRef: Nothing
    }

fakeOutputWithMultiAssets :: MultiAsset -> TransactionOutput
fakeOutputWithMultiAssets =
  fakeOutputWithValue <<< mkValue Coin.zero
