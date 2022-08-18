module Examples.ByUrl (main) where

import Prelude

import Contract.Config
  ( ConfigParams
  , testnetEternlConfig
  , testnetFlintConfig
  , testnetGeroConfig
  , testnetNamiConfig
  )
import Contract.Prelude (fst, traverse_, uncurry)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (last)
import Data.Foldable (lookup)
import Data.Maybe (Maybe(Just))
import Data.String.Common (split)
import Data.String.Pattern (Pattern(Pattern))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (error)
import Examples.AlwaysMints as AlwaysMints
import Examples.AlwaysSucceeds as AlwaysSucceeds
import Examples.Datums as Datums
import Examples.MintsMultipleTokens as MintsMultipleTokens
import Examples.Pkh2Pkh as Pkh2Pkh
import Examples.SignMultiple as SignMultiple
import Examples.Wallet as Wallet

foreign import _queryString :: Effect String

foreign import _writeExampleHTML :: String -> Array String -> Effect Unit

main :: Effect Unit
main = do
  traverse_ (uncurry _writeExampleHTML) $ map ((_ /\ map fst wallets) <<< fst)
    examples
  queryString <- last <<< split (Pattern "?") <$> _queryString
  case split (Pattern ":") <$> queryString of
    Just [ exampleName, walletName ] -> do
      example <- liftMaybe (error $ "unknown example name: " <> exampleName) $
        lookup exampleName examples
      wallet <- liftMaybe (error $ "unknown wallet name: " <> walletName) $
        lookup walletName wallets
      example wallet
    _ -> liftEffect $ Console.error "Error parsing query string"

wallets :: Array (String /\ ConfigParams ())
wallets =
  [ "nami" /\ testnetNamiConfig
  , "gero" /\ testnetGeroConfig
  , "flint" /\ testnetFlintConfig
  , "eternl" /\ testnetEternlConfig
  ]

examples :: Array (String /\ (ConfigParams () -> Effect Unit))
examples =
  [ "AlwaysMints" /\ AlwaysMints.example
  , "AlwaysSucceeds" /\ AlwaysSucceeds.example
  , "Datums" /\ Datums.example
  , "Wallet" /\ Wallet.example
  , "Pkh2Pkh" /\ Pkh2Pkh.example
  , "SignMultiple" /\ SignMultiple.example
  , "MintsMultipleTokens" /\ MintsMultipleTokens.example
  ]
