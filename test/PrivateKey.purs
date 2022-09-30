module Test.Ctl.PrivateKey where

import Prelude

import Contract.Config (testnetConfig)
import Contract.Monad (runContract)
import Contract.Transaction (signTransaction)
import Ctl.Internal.Cardano.Types.Transaction
  ( Transaction(Transaction)
  , TransactionWitnessSet(TransactionWitnessSet)
  , Vkeywitness(Vkeywitness)
  , mkEd25519Signature
  )
import Ctl.Internal.Serialization (publicKeyHash)
import Ctl.Internal.Serialization.Keys (publicKeyFromPrivateKey)
import Ctl.Internal.Wallet.KeyFile
  ( privatePaymentKeyFromFile
  , privatePaymentKeyToFile
  , privateStakeKeyFromFile
  , privateStakeKeyToFile
  )
import Ctl.Internal.Wallet.Spec
  ( PrivatePaymentKeySource(PrivatePaymentKeyFile)
  , PrivateStakeKeySource(PrivateStakeKeyFile)
  , WalletSpec(UseKeys)
  )
import Data.Lens (_2, _Just, (^?))
import Data.Lens.Index (ix)
import Data.Lens.Iso.Newtype (unto)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Just), fromJust)
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Mote (group, test)
import Node.FS.Sync (unlink)
import Partial.Unsafe (unsafePartial)
import Test.Ctl.Fixtures (txFixture1)
import Test.Ctl.TestM (TestPlanM)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(Proxy))

suite :: TestPlanM (Aff Unit) Unit
suite = do
  group "PrivateKey" $ do
    test "privateKeyFromFile" do
      let
        cfg =
          testnetConfig
            { walletSpec = Just $ UseKeys
                ( PrivatePaymentKeyFile
                    "fixtures/test/parsing/PrivateKey/payment.skey"
                )
                ( Just $ PrivateStakeKeyFile
                    "fixtures/test/parsing/PrivateKey/stake.skey"
                )
            }
      runContract cfg do
        mbTx <- signTransaction txFixture1
        let
          mbSignature =
            mbTx ^? _Just
              <<< unto Transaction
              <<< prop (Proxy :: Proxy "witnessSet")
              <<< unto TransactionWitnessSet
              <<< prop (Proxy :: Proxy "vkeys")
              <<< _Just
              <<< ix 0
              <<< unto Vkeywitness
              <<<
                _2
        mbSignature `shouldEqual` Just
          ( unsafePartial $ fromJust $ mkEd25519Signature
              "ed25519_sig1w7nkmvk57r6094j9u85r4pddve0hg3985ywl9yzwecx03aa9fnfspl9zmtngmqmczd284lnusjdwkysgukxeq05a548dyepr6vn62qs744wxz"
          )
    test "privateKeyToFile round-trips" do
      key <- privatePaymentKeyFromFile
        "fixtures/test/parsing/PrivateKey/payment.skey"
      privatePaymentKeyToFile
        "fixtures/test/parsing/PrivateKey/payment_round_trip.skey"
        key
      key2 <- privatePaymentKeyFromFile
        "fixtures/test/parsing/PrivateKey/payment_round_trip.skey"
      liftEffect $ unlink
        "fixtures/test/parsing/PrivateKey/payment_round_trip.skey"
      -- converting to pub key hashes to check equality isn't great
      -- but there aren't Eq instances for PrivateKeys or PublicKeys
      let
        pkh = publicKeyHash $ publicKeyFromPrivateKey (unwrap key)
        pkh2 = publicKeyHash $ publicKeyFromPrivateKey (unwrap key2)
      pkh `shouldEqual` pkh2
    test "stakeKeyToFile round-trips" do
      key <- privateStakeKeyFromFile
        "fixtures/test/parsing/PrivateKey/stake.skey"
      privateStakeKeyToFile
        "fixtures/test/parsing/PrivateKey/stake_round_trip.skey"
        key
      key2 <- privateStakeKeyFromFile
        "fixtures/test/parsing/PrivateKey/stake_round_trip.skey"
      liftEffect $ unlink
        "fixtures/test/parsing/PrivateKey/stake_round_trip.skey"
      let pkh = publicKeyHash $ publicKeyFromPrivateKey (unwrap key)
      let pkh2 = publicKeyHash $ publicKeyFromPrivateKey (unwrap key2)
      pkh `shouldEqual` pkh2
