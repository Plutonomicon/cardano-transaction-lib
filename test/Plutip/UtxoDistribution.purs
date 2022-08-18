module Test.Plutip.UtxoDistribution
  ( ArbitraryUtxoDistr
  , assertContract
  , assertCorrectDistribution
  , assertNoUtxosAtAddress
  , assertNoUtxosAtEnterpriseAddress
  , assertUtxosAtPlutipWalletAddress
  , checkUtxoDistribution
  , genInitialUtxo
  , ppArbitraryUtxoDistr
  , suite
  , withArbUtxoDistr
  ) where

import Prelude

import Contract.Address
  ( Address
  , getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , ownStakePubKeyHash
  , payPubKeyHashEnterpriseAddress
  )
import Contract.Monad (Contract, liftedM)
import Contract.Test.Plutip
  ( class UtxoDistribution
  , InitialUTxOs
  , runPlutipContract
  )
import Contract.Transaction
  ( TransactionInput
  , TransactionOutput(TransactionOutput)
  )
import Contract.Utxos (utxosAt)
import Contract.Value (Value, lovelaceValueOf)
import Contract.Wallet (KeyWallet, withKeyWallet)
import Control.Lazy (fix)
import Data.Array (foldl, zip)
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, toString) as BigInt
import Data.Foldable (intercalate)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (fromFoldable) as List
import Data.Map (empty, insert, isEmpty) as Map
import Data.Maybe (isJust)
import Data.Newtype (unwrap, wrap)
import Data.NonEmpty ((:|))
import Data.Traversable (for_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Mote (group, test)
import Plutip.Types (InitialUTxOsWithStakeKey(InitialUTxOsWithStakeKey))
import Plutip.UtxoDistribution (encodeDistribution, keyWallets)
import Plutus.Types.Transaction (Utxo)
import Test.Plutip.Common (config, privateStakeKey)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen
  ( Gen
  , arrayOf
  , chooseInt
  , frequency
  , randomSample'
  , resize
  , sized
  )
import TestM (TestPlanM)
import Type.Prelude (Proxy(Proxy))

suite :: TestPlanM Unit
suite = group "Plutip UtxoDistribution" do
  distrs <- liftEffect $ randomSample' 5 arbitrary
  for_ distrs $ \distr ->
    test
      ( "runPlutipContract: stake key transfers with random distribution: "
          <> ppArbitraryUtxoDistr distr
      )
      $
        withArbUtxoDistr
          distr
          \randDistr -> runPlutipContract config randDistr $
            checkUtxoDistribution randDistr

checkUtxoDistribution
  :: forall distr wallet (r :: Row Type)
   . UtxoDistribution distr wallet
  => distr
  -> wallet
  -> Contract r Unit
checkUtxoDistribution distr wallets = do
  let
    walletsArray = keyWallets (Proxy :: Proxy distr) wallets
    walletUtxos = encodeDistribution distr
  for_ walletsArray assertUtxosAtPlutipWalletAddress
  assertCorrectDistribution $ zip walletsArray walletUtxos

-- TODO: minimum value of 1 ada is hardcoded, tests become flaky below
-- that value. Ideally this shouldn't be hardcoded. We might be able
-- to remove this minimum after
-- https://github.com/Plutonomicon/cardano-transaction-lib/issues/857
-- is resolved
genInitialUtxo :: Gen InitialUTxOs
genInitialUtxo = map (BigInt.fromInt >>> (_ * BigInt.fromInt 1_000_000))
  <$> arrayOf (chooseInt 1 1000)

instance Arbitrary ArbitraryUtxoDistr where
  arbitrary = fix \_ -> sized $ \size -> resize size $ frequency <<< wrap $
    (1.0 /\ pure UDUnit) :|
      List.fromFoldable
        [ 2.0 /\ (UDInitialUtxos <$> genInitialUtxo)
        , 2.0 /\
            ( UDInitialUtxosWithStake <$>
                ( InitialUTxOsWithStakeKey
                    <$> (pure privateStakeKey)
                    <*> genInitialUtxo
                )
            )
        , 4.0 /\
            ( UDTuple
                <$> resize (size - 1) arbitrary
                <*> resize (size - 1) arbitrary
            )
        ]

data ArbitraryUtxoDistr
  = UDUnit
  | UDInitialUtxos InitialUTxOs
  | UDInitialUtxosWithStake InitialUTxOsWithStakeKey
  | UDTuple ArbitraryUtxoDistr ArbitraryUtxoDistr

ppInitialUtxos :: InitialUTxOs -> String
ppInitialUtxos x = "[" <> intercalate ", " (map BigInt.toString x) <> "]"

ppArbitraryUtxoDistr :: ArbitraryUtxoDistr -> String
ppArbitraryUtxoDistr = case _ of
  UDUnit -> "unit"
  UDInitialUtxos x -> ppInitialUtxos x
  UDInitialUtxosWithStake (InitialUTxOsWithStakeKey _ x) ->
    "stake + " <> ppInitialUtxos x
  UDTuple x y -> "(" <> ppArbitraryUtxoDistr x <> " /\\ "
    <> ppArbitraryUtxoDistr y
    <> ")"

withArbUtxoDistr
  :: forall a
   . ArbitraryUtxoDistr
  -> (forall distr wallet. UtxoDistribution distr wallet => distr -> a)
  -> a
withArbUtxoDistr d f = case d of
  UDUnit -> f unit
  UDInitialUtxos x -> f x
  UDInitialUtxosWithStake x -> f x
  UDTuple x y ->
    withArbUtxoDistr x (\d1 -> withArbUtxoDistr y (f <<< (d1 /\ _)))

assertContract :: forall (r :: Row Type). String -> Boolean -> Contract r Unit
assertContract msg cond = if cond then pure unit else liftEffect $ throw msg

-- | For a plutip test wallet, assert that any utxos held by the
-- | wallet are at the expected address. If the wallet has a stake
-- | key, this function assumes the expected address is the base
-- | address, otherwise it assumes the expected address is the
-- | enterprise address.
assertUtxosAtPlutipWalletAddress
  :: forall (r :: Row Type). KeyWallet -> Contract r Unit
assertUtxosAtPlutipWalletAddress wallet = withKeyWallet wallet do
  maybeStake <- ownStakePubKeyHash
  when (isJust maybeStake) $ assertNoUtxosAtEnterpriseAddress wallet

assertNoUtxosAtEnterpriseAddress
  :: forall (r :: Row Type). KeyWallet -> Contract r Unit
assertNoUtxosAtEnterpriseAddress wallet = withKeyWallet wallet $
  assertNoUtxosAtAddress =<< liftedM "Could not get wallet address"
    ( payPubKeyHashEnterpriseAddress
        <$> getNetworkId
        <*> liftedM "Could not get payment pubkeyhash" ownPaymentPubKeyHash
    )

assertNoUtxosAtAddress :: forall (r :: Row Type). Address -> Contract r Unit
assertNoUtxosAtAddress addr = do
  utxos <- liftedM "Could not get wallet utxos" $ map unwrap <$> utxosAt addr
  assertContract "Expected address to not hold utxos" $ Map.isEmpty utxos

-- | For each wallet, assert that there is a one-to-one correspondance
-- | between its utxo set and its expected utxo amounts.
assertCorrectDistribution
  :: forall (r :: Row Type)
   . Array (KeyWallet /\ InitialUTxOs)
  -> Contract r Unit
assertCorrectDistribution wallets = for_ wallets \(wallet /\ expectedAmounts) ->
  withKeyWallet wallet do
    addr <- liftedM "Could not get wallet address" getWalletAddress
    utxos <- liftedM "Could not get wallet utxos" $ map unwrap <$> utxosAt addr
    assertContract "Incorrect distribution of utxos" $
      checkDistr utxos expectedAmounts
  where
  -- Idea here is to iterate through the expected amounts and remove
  -- one matching utxo from the utxo set if found, otherwise return
  -- false. Once we've gone through all expected amounts, if all of
  -- them have been found in the utxo set, we expect there to be no
  -- utxos remaining
  checkDistr :: Utxo -> InitialUTxOs -> Boolean
  checkDistr originalUtxos expectedAmounts =
    let
      allFound /\ remainingUtxos =
        foldl findAndRemoveExpected (true /\ originalUtxos) expectedAmounts
    in
      allFound && Map.isEmpty remainingUtxos
    where
    -- Remove a single utxo containing the expected ada amount,
    -- returning the updated utxo map and false if it could not be
    -- found
    findAndRemoveExpected :: Boolean /\ Utxo -> BigInt -> Boolean /\ Utxo
    findAndRemoveExpected o@(false /\ _) _ = o
    findAndRemoveExpected (_ /\ utxos) expected =
      foldlWithIndex
        (removeUtxoMatchingValue $ lovelaceValueOf expected)
        (false /\ Map.empty)
        utxos

    -- Include the utxo if it does not match the value, return true if
    -- the utxo matches the value
    removeUtxoMatchingValue
      :: Value
      -> TransactionInput
      -> Boolean /\ Utxo
      -> TransactionOutput
      -> Boolean /\ Utxo
    removeUtxoMatchingValue
      expected
      i
      (found /\ m)
      o@(TransactionOutput { amount })
      | not found && expected == amount = true /\ m
      | otherwise = found /\ Map.insert i o m
