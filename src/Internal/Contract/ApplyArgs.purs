module Ctl.Internal.Contract.ApplyArgs
  ( applyArgs
  ) where

import Prelude

import Ctl.Internal.QueryM (ClientError(..), scriptToAeson, postAeson, handleAffjaxResponse)
import Ctl.Internal.Contract.Monad(Contract)
import Aeson
  ( Aeson
  , encodeAeson
  )
import Control.Monad.Reader.Trans
  ( asks
  )
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.QueryM.ServerConfig
  ( mkHttpUrl
  )
import Ctl.Internal.Serialization (toBytes) as Serialization
import Ctl.Internal.Serialization.PlutusData (convertPlutusData) as Serialization
import Ctl.Internal.Types.ByteArray (byteArrayToHex)
import Ctl.Internal.Types.PlutusData (PlutusData)
import Ctl.Internal.Types.Scripts (Language, PlutusScript(PlutusScript))
import Data.Either (Either(Left))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple), snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (liftAff)
import Foreign.Object as Object
import Untagged.Union (asOneOf)

-- | Apply `PlutusData` arguments to any type isomorphic to `PlutusScript`,
-- | returning an updated script with the provided arguments applied
applyArgs
  :: PlutusScript
  -> Array PlutusData
  -> Contract (Either ClientError PlutusScript)
applyArgs script args =
  asks _.ctlServerConfig >>= case _ of
    Nothing -> pure
      $ Left
      $
        ClientOtherError
          "The `ctl-server` service is required to call `applyArgs`. Please \
          \provide a `Just` value in `ConfigParams.ctlServerConfig` and make \
          \sure that the `ctl-server` service is running and available at the \
          \provided host and port. The `ctl-server` packages can be obtained \
          \from `overlays.ctl-server` defined in CTL's flake. Please see \
          \`doc/runtime.md` in the CTL repository for more information"
    Just config -> case traverse plutusDataToAeson args of
      Nothing -> pure $ Left $ ClientEncodingError
        "Failed to convert script args"
      Just ps -> do
        let
          language :: Language
          language = snd $ unwrap script

          url :: String
          url = mkHttpUrl config <</>> "apply-args"

          reqBody :: Aeson
          reqBody = encodeAeson
            $ Object.fromFoldable
                [ "script" /\ scriptToAeson script
                , "args" /\ encodeAeson ps
                ]
        liftAff (postAeson url reqBody)
          <#> map (PlutusScript <<< flip Tuple language) <<<
            handleAffjaxResponse
  where
  plutusDataToAeson :: PlutusData -> Maybe Aeson
  plutusDataToAeson =
    map
      ( encodeAeson
          <<< byteArrayToHex
          <<< Serialization.toBytes
          <<< asOneOf
      )
      <<< Serialization.convertPlutusData
