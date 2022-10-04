module Ctl.Examples.Demo.HTML
  ( onSend
  , setBalance
  , OnSendHandler
  , Amount
  , SetStatus
  , SetBuilt
  , TxId
  , Fee
  , SetConfirmedAndAwaitRetrieve
  , SetRetrieveConfirmed
  ) where

import Prelude

import Contract.Monad (Contract)
import Contract.Value (Value, valueToCoin')
import Data.BigInt (fromInt, toString)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.AVar as Effect.AVar
import Effect.Aff (makeAff, parallel, sequential)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)

makeAff' :: forall a. ((a -> Effect Unit) -> Effect Unit) -> Contract () a
makeAff' f = liftAff $ makeAff \g -> f (g <<< pure) $> mempty

type Fee = Number
type TxId = String
type Amount = Number

type SetRetrieveConfirmedE = TxId -> Fee -> Effect Unit
type OnRetrieveHandlerE = SetRetrieveConfirmedE -> Effect Unit
type OnRetrieveE = OnRetrieveHandlerE -> Effect Unit
type SetConfirmedAndOnRetrieveE = OnRetrieveE -> Effect Unit
type SetBuiltE = TxId -> Fee -> SetConfirmedAndOnRetrieveE -> Effect Unit
type SetStatusE = String -> Effect Unit
type OnSendHandlerE = Amount -> SetStatusE -> SetBuiltE -> Effect Unit

foreign import _onSend :: OnSendHandlerE -> Effect Unit

foreign import _setBalance :: String -> Effect Unit

type SetRetrieveConfirmed = TxId -> Fee -> Contract () Unit
type SetConfirmedAndAwaitRetrieve = Contract () SetRetrieveConfirmed
type SetBuilt = TxId -> Fee -> Contract () SetConfirmedAndAwaitRetrieve
type SetStatus = String -> Contract () Unit
type OnSendHandler = Amount -> SetStatus -> SetBuilt -> Contract () Unit

-- | Sets up a handler which listens for send form to be submitted. The handler
-- | recieves a status setter and a sequence of actions to perform, to update
-- | the document and eventually wait for the retrieve button to be clicked.
-- | Each action should only be performed once, other than the status setter.
-- | Requires `./examples/Demo/index.html` to be the window HTML.
-- An indexed monad would be a good idea here.  We can hide the returned actions
-- inside state, and the index proves whats in the state.
onSend :: OnSendHandler -> Contract () Unit
onSend handler = do
  avar <- liftAff $ AVar.empty
  liftEffect $ _onSend \amount setStatus cont -> void $ Effect.AVar.tryPut
    (amount /\ setStatus /\ cont)
    avar
  let
    listen :: Contract () Unit
    listen = (liftAff $ AVar.take avar) >>=
      \(amount /\ setStatus /\ setBuilt) ->
        do
          sequential ado
            _ <- parallel listen
            _ <- parallel $ handler amount (liftEffect <<< setStatus)
              \txid fee ->
                do
                  setConfirmed <- makeAff' (setBuilt txid fee)
                  pure do
                    onRetrieve <- makeAff' setConfirmed
                    pure \a b -> liftEffect (onRetrieve a b)
            in unit
  listen

setBalance :: Value -> Contract () Unit
setBalance balance = do
  let balanceS = toString $ (valueToCoin' balance / fromInt 1_000_000)
  liftEffect $ _setBalance balanceS
