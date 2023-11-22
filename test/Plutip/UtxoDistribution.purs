module Test.Ctl.Plutip.UtxoDistribution
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
  , payPubKeyHashEnterpriseAddress
  )
import Contract.Monad (Contract, liftedM)
import Contract.Test.Plutip
  ( class UtxoDistribution
  , InitialUTxOs
  , InitialUTxOsWithStakeKey(InitialUTxOsWithStakeKey)
  , runPlutipContract
  , withStakeKey
  )
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Contract.Utxos (UtxoMap, utxosAt)
import Contract.Value (Value, lovelaceValueOf)
import Contract.Wallet
  ( KeyWallet
  , getWalletAddresses
  , ownPaymentPubKeyHashes
  , ownStakePubKeyHashes
  , withKeyWallet
  )
import Control.Lazy (fix)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Ctl.Internal.Test.UtxoDistribution (encodeDistribution, keyWallets)
import Data.Array (foldl, head, replicate, zip)
import Data.Array.NonEmpty (fromNonEmpty) as NEArray
import Data.Foldable (intercalate)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (empty, insert, isEmpty) as Map
import Data.Maybe (isJust)
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))
import Data.Traversable (for_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import JS.BigInt (BigInt)
import JS.BigInt (fromInt, toString) as BigInt
import Mote (group, test)
import Test.Ctl.Plutip.Common (config, privateStakeKey)
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
import Type.Prelude (Proxy(Proxy))

suite :: TestPlanM (Aff Unit) Unit
suite = group "UtxoDistribution" do
  test
    "stake key transfers with distribution: [[1000000000,1000000000]]"
    do
      let
        distribution :: Array InitialUTxOs
        distribution = replicate 2 [ BigInt.fromInt 1_000_000_000 ]
      runPlutipContract config distribution $ checkUtxoDistribution distribution

  test
    "stake key transfers with distribution: stake + [[1000000000,1000000000]]"
    do
      let
        distribution :: Array InitialUTxOsWithStakeKey
        distribution = withStakeKey privateStakeKey <$> replicate 2
          [ BigInt.fromInt 1_000_000_000 ]
      runPlutipContract config distribution $ checkUtxoDistribution distribution

  test
    "stake key transfers with distribution: ([[1000000000,1000000000]], stake + [[1000000000,1000000000]])"
    do
      let
        distribution1 :: Array InitialUTxOs
        distribution1 = replicate 2 [ BigInt.fromInt 1_000_000_000 ]

        distribution = distribution1 /\
          (withStakeKey privateStakeKey <$> distribution1)
      runPlutipContract config distribution $ checkUtxoDistribution distribution

  distrs <- liftEffect $ randomSample' 5 arbitrary
  for_ distrs $ \distr ->
    test
      ( "stake key transfers with random distribution: "
          <> ppArbitraryUtxoDistr distr
      )
      $
        withArbUtxoDistr
          distr
          \randDistr -> runPlutipContract config randDistr $
            checkUtxoDistribution randDistr

checkUtxoDistribution
  :: forall (distr :: Type) (wallet :: Type)
   . UtxoDistribution distr wallet
  => distr
  -> wallet
  -> Contract Unit
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
  arbitrary =
    fix \_ -> sized $ \size -> resize size $ frequency $ NEArray.fromNonEmpty $
      (1.0 /\ pure UDUnit) :|
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

-- TODO Add UDArray
-- https://github.com/Plutonomicon/cardano-transaction-lib/issues/1187
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

assertContract :: String -> Boolean -> Contract Unit
assertContract msg cond = if cond then pure unit else liftEffect $ throw msg

-- | For a plutip test wallet, assert that any utxos held by the
-- | wallet are at the expected address. If the wallet has a stake
-- | key, this function assumes the expected address is the base
-- | address, otherwise it assumes the expected address is the
-- | enterprise address.
assertUtxosAtPlutipWalletAddress
  :: KeyWallet -> Contract Unit
assertUtxosAtPlutipWalletAddress wallet = withKeyWallet wallet do
  maybeStake <- join <<< head <$> ownStakePubKeyHashes
  when (isJust maybeStake) $ assertNoUtxosAtEnterpriseAddress wallet

assertNoUtxosAtEnterpriseAddress
  :: KeyWallet -> Contract Unit
assertNoUtxosAtEnterpriseAddress wallet = withKeyWallet wallet $
  assertNoUtxosAtAddress =<< liftedM "Could not get wallet address"
    ( payPubKeyHashEnterpriseAddress
        <$> getNetworkId
        <*> liftedM "Could not get payment pubkeyhash"
          (head <$> ownPaymentPubKeyHashes)
    )

assertNoUtxosAtAddress :: Address -> Contract Unit
assertNoUtxosAtAddress addr = do
  utxos <- utxosAt addr
  assertContract "Expected address to not hold utxos" $ Map.isEmpty utxos

-- | For each wallet, assert that there is a one-to-one correspondance
-- | between its utxo set and its expected utxo amounts.
assertCorrectDistribution
  :: Array (KeyWallet /\ InitialUTxOs)
  -> Contract Unit
assertCorrectDistribution wallets = for_ wallets \(wallet /\ expectedAmounts) ->
  withKeyWallet wallet do
    addr <- liftedM "Could not get wallet address" $ head <$> getWalletAddresses
    utxos <- utxosAt addr
    assertContract "Incorrect distribution of utxos" $
      checkDistr utxos expectedAmounts
  where
  -- Idea here is to iterate through the expected amounts and remove
  -- one matching utxo from the utxo set if found, otherwise return
  -- false. Once we've gone through all expected amounts, if all of
  -- them have been found in the utxo set, we expect there to be no
  -- utxos remaining
  checkDistr :: UtxoMap -> InitialUTxOs -> Boolean
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
    findAndRemoveExpected :: Boolean /\ UtxoMap -> BigInt -> Boolean /\ UtxoMap
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
      -> Boolean /\ UtxoMap
      -> TransactionOutputWithRefScript
      -> Boolean /\ UtxoMap
    removeUtxoMatchingValue
      expected
      i
      (found /\ m)
      o@(TransactionOutputWithRefScript { output })
      | not found && expected == (unwrap output).amount = true /\ m
      | otherwise = found /\ Map.insert i o m
