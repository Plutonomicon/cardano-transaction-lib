module Types.Ada
  ( Ada(Lovelace)
  , fromValue
  , getLovelace
  , lovelaceValueOf
  , toValue
  ) where

import Prelude
import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

import Types.Value
  ( adaSymbol
  , adaToken
  , Value(Value)
  , valueOf
  )

-- Replicating Ada from Plutus, not sure how useful necessary this will be in practice:
-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Ada.html
-- | ADA, the special currency on the Cardano blockchain. The unit of Ada is Lovelace, and
--   1M Lovelace is one Ada.
--   See note [Currencies] in 'Ledger.Validation.Value.TH'.
newtype Ada = Lovelace BigInt
derive instance genericAda :: Generic Ada _
derive instance newtypeAda :: Newtype Ada _
derive newtype instance eqAda :: Eq Ada
derive newtype instance ordAda :: Ord Ada

instance showAda :: Show Ada where
  show = genericShow

instance semigroupAda :: Semigroup Ada where
  append (Lovelace a1) (Lovelace a2) = Lovelace (a1 + a2)

instance monoidAda :: Monoid Ada where
  mempty = Lovelace zero

getLovelace :: Ada -> BigInt
getLovelace = unwrap

lovelaceValueOf :: BigInt -> Value
lovelaceValueOf = flip (Value <<< wrap) mempty

-- | Create a 'Value' containing only the given 'Ada'.
toValue :: Ada -> Value
toValue (Lovelace i) = lovelaceValueOf i

-- | Get the 'Ada' in the given 'Value'.
fromValue :: Value -> Ada
fromValue v = Lovelace (valueOf v adaSymbol adaToken)
