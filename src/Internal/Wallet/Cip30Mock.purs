module Ctl.Internal.Wallet.Cip30Mock where

import Prelude

import Contract.Monad (Contract, ContractEnv, wrapContract)
import Control.Monad.Error.Class (liftMaybe, try)
import Control.Monad.Reader (ask)
import Control.Monad.Reader.Class (local)
import Control.Promise (Promise, fromAff)
import Ctl.Internal.Cardano.Types.Transaction
  ( TransactionOutput(TransactionOutput)
  )
import Ctl.Internal.Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  )
import Ctl.Internal.Cardano.Types.Value
  ( Coin(Coin)
  , Value
  , coinToValue
  , geq
  , gt
  , lovelaceValueOf
  )
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.Deserialization.Transaction (deserializeTransaction)
import Ctl.Internal.Deserialization.UnspentOutput (convertValue) as DSV
import Ctl.Internal.Helpers (liftEither)
import Ctl.Internal.QueryM (QueryM, runQueryMInRuntime)
import Ctl.Internal.QueryM.Utxos (utxosAt)
import Ctl.Internal.Serialization
  ( convertTransactionUnspentOutput
  , convertValue
  , toBytes
  )
import Ctl.Internal.Serialization.Address (NetworkId(TestnetId, MainnetId))
import Ctl.Internal.Serialization.WitnessSet (convertWitnessSet)
import Ctl.Internal.Types.BigNum as BigNum
import Ctl.Internal.Types.ByteArray (byteArrayToHex, hexToByteArray)
import Ctl.Internal.Types.CborBytes
  ( cborBytesFromByteArray
  , cborBytesToHex
  , hexToCborBytes
  )
import Ctl.Internal.Wallet
  ( Wallet
  , WalletExtension(LodeWallet, NamiWallet, GeroWallet, FlintWallet, NuFiWallet)
  , mkWalletAff
  )
import Ctl.Internal.Wallet.Cip30 (Paginate)
import Ctl.Internal.Wallet.Key
  ( KeyWallet(KeyWallet)
  , PrivatePaymentKey
  , PrivateStakeKey
  , privateKeysToKeyWallet
  )
import Data.Array (length)
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Either (Either(Right, Left), hush)
import Data.Foldable (fold, foldMap)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Int (ceil, toNumber)
import Data.Lens ((.~))
import Data.Lens.Common (simple)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Unsafe (unsafePerformEffect)
import Literals.Null (Null, null)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(Proxy))
import Untagged.Castable (class Castable)
import Untagged.Union (OneOf, UndefinedOr, asOneOf, uorToMaybe)

data WalletMock = MockFlint | MockGero | MockNami | MockLode | MockNuFi

data PaginateError = PaginateError Int

derive instance Eq PaginateError

instance Show PaginateError where
  show (PaginateError maxPages) = "PaginateError " <> show maxPages

data InvalidRequestError = InvalidRequestError String

-- | Construct a CIP-30 wallet mock that exposes `KeyWallet` functionality
-- | behind a CIP-30 interface and uses Ogmios to submit Txs.
-- | The wallet is injected directly to `window.cardano` object, under the
-- | name corresponding to provided `WalletMock`. It works even in NodeJS
-- | (we introduce a global `window` object and delete it afterwards).
-- |
-- | Note that this function will refuse to overwrite existing wallet
-- | with the mock, so the users should disable their browser extensions
-- | before running it in the browser.
-- |
-- | Note that this function implements single-address light wallet logic, so
-- | it will have to be changed a lot to successfully mimic the behavior of
-- | multi-address wallets, like Eternl.
withCip30Mock
  :: forall (r :: Row Type) (a :: Type)
   . KeyWallet
  -> WalletMock
  -> Contract r a
  -> Contract r a
withCip30Mock (KeyWallet keyWallet) mock contract = do
  cip30Mock <- wrapContract $ mkCip30Mock keyWallet.paymentKey
    keyWallet.stakeKey
  deleteMock <- liftEffect $ injectCip30Mock mockString cip30Mock
  wallet <- liftAff mkWalletAff'
  let
    setUpdatedWallet :: ContractEnv r -> ContractEnv r
    setUpdatedWallet =
      simple _Newtype <<< prop (Proxy :: Proxy "runtime") <<< prop
        (Proxy :: Proxy "wallet") .~
        (Just wallet)
  res <- try $ local setUpdatedWallet contract
  liftEffect deleteMock
  liftEither res
  where
  mkWalletAff' :: Aff Wallet
  mkWalletAff' = case mock of
    MockFlint -> mkWalletAff FlintWallet
    MockGero -> mkWalletAff GeroWallet
    MockNami -> mkWalletAff NamiWallet
    MockLode -> mkWalletAff LodeWallet
    MockNuFi -> mkWalletAff NuFiWallet

  mockString :: String
  mockString = case mock of
    MockFlint -> "flint"
    MockGero -> "gerowallet"
    MockNami -> "nami"
    MockLode -> "LodeWallet"
    MockNuFi -> "nufi"

type CollateralParams = { amount :: String }

type Cip30Mock =
  { getNetworkId :: Effect (Promise Int)
  , getUtxos ::
      Fn2 (UndefinedOr String) (UndefinedOr Paginate)
        (Promise (NullOr (Array String)))
  , getCollateral :: CollateralParams -> ((Promise (Array String)))
  , getBalance :: Effect (Promise String)
  , getUsedAddresses :: (UndefinedOr Paginate) -> Promise (Array String)
  , getUnusedAddresses :: Effect (Promise (Array String))
  , getChangeAddress :: Effect (Promise String)
  , getRewardAddresses :: Effect (Promise (Array String))
  , signTx :: Fn2 String Boolean (Promise String)
  , signData ::
      Fn2 String String (Promise { key :: String, signature :: String })
  }

-- | By CIP-30 collateral required amount must be not more than 5 ADA
maxCollateralAmount :: Value
maxCollateralAmount = lovelaceValueOf
  $ unsafePartial
  $ fromJust
  $ BigInt.fromString "5000000000"

mkCip30Mock
  :: PrivatePaymentKey -> Maybe PrivateStakeKey -> QueryM Cip30Mock
mkCip30Mock pKey mSKey = do
  { config, runtime } <- ask
  let
    getCollateralUtxos utxos = do
      let
        pparams = unwrap $ runtime.pparams
        coinsPerUtxoUnit = pparams.coinsPerUtxoUnit
        maxCollateralInputs = UInt.toInt $
          pparams.maxCollateralInputs
      liftEffect $
        (unwrap keyWallet).selectCollateral coinsPerUtxoUnit
          maxCollateralInputs
          utxos
          <#> fold

  let
    convertAmount :: String -> Effect Value
    convertAmount amount = do
      let
        mAmountCoin = Coin <$>
          (BigNum.toBigInt <$> BigNum.fromString amount)
      amountCoin <- case mAmountCoin of
        Nothing -> liftEffect $ raiseInvalidRequestError
          "amount param has incorrect format"
        Just x -> pure x
      pure $ coinToValue amountCoin
  pure $
    { getNetworkId: fromAff $ pure $
        case config.networkId of
          TestnetId -> 0
          MainnetId -> 1
    , getUtxos: mkFn2 \amount pagination -> unsafePerformEffect $ fromAff do
        ownAddress <- (unwrap keyWallet).address config.networkId
        utxos <- liftMaybe (error "No UTxOs at address") =<<
          runQueryMInRuntime config runtime (utxosAt ownAddress)
        collateralUtxos <- getCollateralUtxos utxos
        let
          -- filter UTxOs that will be used as collateral
          nonCollateralUtxos =
            Map.filter
              (flip Array.elem (collateralUtxos <#> unwrap >>> _.output))
              utxos
        let
          xUtxos = Map.toUnfoldable nonCollateralUtxos <#> \(input /\ output) ->
            TransactionUnspentOutput { input, output }
        let
          amountValue = DSV.convertValue =<< fromBytes =<< hexToCborBytes =<<
            (uorToMaybe amount)
        if (not hasEnoughAmount amountValue xUtxos) then pure $ maybeToNullOr
          Nothing
        else do
          -- Convert to CSL representation and serialize
          cslUtxos <- traverse (liftEffect <<< convertTransactionUnspentOutput)
            xUtxos
          page <- liftEffect
            $ paginateArray (uorToMaybe pagination)
            $
              (cborBytesToHex <<< toBytes) <$> cslUtxos
          pure $ maybeToNullOr $ Just page
    , getCollateral: \{ amount } -> unsafePerformEffect $ fromAff do
        ownAddress <- (unwrap keyWallet).address config.networkId
        utxos <- liftMaybe (error "No UTxOs at address") =<<
          runQueryMInRuntime config runtime (utxosAt ownAddress)
        collateralUtxos <- getCollateralUtxos utxos
        amountValue <- liftEffect $ convertAmount amount
        if gt amountValue maxCollateralAmount then liftEffect $
          raiseInvalidRequestError "Amount value is bigger than allowed 5 ADA"
        else pure unit
        if (not $ hasEnoughAmount (Just amountValue) collateralUtxos) then
          liftEffect $ raiseInvalidRequestError
            "Cannot find collateral to match required amount"
        else do
          cslUnspentOutput <- liftEffect $ traverse
            convertTransactionUnspentOutput
            collateralUtxos
          pure $ (cborBytesToHex <<< toBytes) <$> cslUnspentOutput
    , getBalance: fromAff do
        ownAddress <- (unwrap keyWallet).address config.networkId
        utxos <- liftMaybe (error "No UTxOs at address") =<<
          runQueryMInRuntime config runtime (utxosAt ownAddress)
        value <- liftEffect $ convertValue $
          (foldMap (_.amount <<< unwrap) <<< Map.values)
            utxos
        pure $ cborBytesToHex $ toBytes value
    , getUsedAddresses: \pagination -> unsafePerformEffect $ fromAff do
        result <- (unwrap keyWallet).address config.networkId <#> \address ->
          [ (cborBytesToHex <<< toBytes) address ]
        liftEffect $ paginateArray (uorToMaybe pagination) result
    , getUnusedAddresses: fromAff $ pure []
    , getChangeAddress: fromAff do
        (unwrap keyWallet).address config.networkId <#>
          byteArrayToHex <<< unwrap <<< toBytes
    , getRewardAddresses: fromAff do
        (unwrap keyWallet).address config.networkId <#> \address ->
          [ (cborBytesToHex <<< toBytes) address ]
    , signTx: mkFn2 \str _partialSign -> unsafePerformEffect $ fromAff do
        txBytes <- liftMaybe (error "Unable to convert CBOR") $ hexToCborBytes
          str
        tx <- liftMaybe (error "Failed to decode Transaction CBOR")
          $ hush
          $ deserializeTransaction
          $ txBytes
        witness <- (unwrap keyWallet).signTx tx
        cslWitnessSet <- liftEffect $ convertWitnessSet witness
        pure $ cborBytesToHex $ toBytes cslWitnessSet
    , signData: mkFn2 \_addr msg -> unsafePerformEffect $ fromAff do
        msgBytes <- liftMaybe (error "Unable to convert CBOR") $
          hexToByteArray msg
        { key, signature } <- (unwrap keyWallet).signData config.networkId
          (wrap msgBytes)
        pure { key: cborBytesToHex key, signature: cborBytesToHex signature }
    }
  where
  keyWallet = privateKeysToKeyWallet pKey mSKey

-- returns an action that removes the mock.
foreign import injectCip30Mock :: String -> Cip30Mock -> Effect (Effect Unit)

-- Helpers

foreign import raisePaginateError :: forall a. Int -> Effect a

type EitherErrorFfi error errorParam return =
  { right :: return -> Either error return
  , left :: errorParam -> Either error return
  }

eitherPaginateErrorFfi :: forall a. EitherErrorFfi PaginateError Int a
eitherPaginateErrorFfi = { right: Right, left: \x -> Left $ PaginateError x }

foreign import _catchPaginateError
  :: forall a
   . EitherErrorFfi PaginateError Int a
  -> Effect a
  -> Effect (Either PaginateError a)

catchPaginateError :: forall a. Effect a -> Effect (Either PaginateError a)
catchPaginateError = _catchPaginateError eitherPaginateErrorFfi

foreign import raiseInvalidRequestError :: String -> forall a. Effect a

paginateArray :: forall a. Maybe Paginate -> Array a -> Effect (Array a)
paginateArray (Just { page, limit }) xs =
  case page >= maxPages of
    true -> raisePaginateError maxPages
    false -> pure $ Array.slice startAt endAt xs
  where
  startAt = page * limit
  endAt = (page + 1) * limit
  maxPages = ceil $ (toNumber (length xs)) / (toNumber limit)
paginateArray Nothing xs = pure xs

hasEnoughAmount :: Maybe Value -> Array TransactionUnspentOutput -> Boolean
hasEnoughAmount Nothing _ = true
hasEnoughAmount (Just amountRequired) values = geq sumAmount amountRequired
  where
  utxoAmount (TransactionUnspentOutput { output }) = outputAmount output
  outputAmount (TransactionOutput { amount }) = amount
  sumAmount = fold $ map utxoAmount values

-- Support NullOr

type NullOr x = OneOf Null x

maybeToNullOr :: forall a. Castable a (OneOf Null a) => Maybe a -> NullOr a
maybeToNullOr (Just x) = asOneOf x
maybeToNullOr Nothing = asOneOf null
