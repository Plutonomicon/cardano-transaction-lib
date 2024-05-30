module Internal.Testnet.Types
  ( CardanoTestnetStartupParams
  , Era (..)
  , LoggingFormat (..)
  , OptionalStartupParams
  , defaultOptionalStartupParams
  ) where


import Contract.Prelude
import Data.Time.Duration (Milliseconds, Seconds)

data Era
  = Byron
  | Shelley
  | Allegra
  | Mary
  | Alonzo
  | Babbage

data LoggingFormat = LogAsJson | LogAsText

type OptionalStartupParams =
  ( numPoolNodes :: Maybe Int
  , era :: Maybe Era
  , epochLength :: Maybe Milliseconds
  , slotLength :: Maybe Seconds
  )

-- | Command line params for the cardano-testnet executable
type CardanoTestnetStartupParams =
  { testnetMagic :: Int
  , activeSlotsCoeff :: Number
  , enableP2p :: Boolean
  , nodeLoggingFormat :: LoggingFormat
  | OptionalStartupParams
  }

defaultOptionalStartupParams :: Record OptionalStartupParams 
defaultOptionalStartupParams =
  { numPoolNodes: Nothing
  , era: Nothing
  , epochLength: Nothing
  , slotLength: Nothing
  }