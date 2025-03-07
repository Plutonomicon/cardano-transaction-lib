-- | This module defines utilities for working with HTTP requests
module Ctl.Internal.QueryM.HttpUtils
  ( handleAffjaxResponseGeneric
  ) where

import Prelude

import Aeson (JsonDecodeError)
import Affjax (Error, Response) as Affjax
import Affjax.StatusCode as Affjax.StatusCode
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right))

--------------------------------------------------------------------------------
-- Affjax
--------------------------------------------------------------------------------

type AffjaxResponseHandler err intermediate result =
  { httpError :: Affjax.Error -> err
  -- ^ Convert an Affjax error into custom error
  , httpStatusCodeError :: Int -> String -> err
  -- ^ Convert a non-2xx status code into custom error
  , decodeError :: String -> JsonDecodeError -> err
  -- ^ Wrap aeson-parse/decode errors
  , parse :: String -> Either JsonDecodeError intermediate
  -- ^ Parse the response body
  , transform :: intermediate -> Either err result
  -- ^ Function from `intermediate` to `result`
  }

-- Checks response status code and returns `ClientError` in case of failure,
-- otherwise attempts to decode the result.
--
-- This function solves the problem described there:
-- https://github.com/eviefp/purescript-affjax-errors

handleAffjaxResponseGeneric
  :: forall err intermediate result
   . AffjaxResponseHandler err intermediate result
  -> Either Affjax.Error (Affjax.Response String)
  -> Either err result
handleAffjaxResponseGeneric handler =
  case _ of
    Left affjaxError ->
      Left (handler.httpError affjaxError)
    Right { status: Affjax.StatusCode.StatusCode statusCode, body }
      | statusCode < 200 || statusCode > 299 ->
          Left (handler.httpStatusCodeError statusCode body)
      | otherwise -> do
          intermediate <- lmap (handler.decodeError body) do
            handler.parse body
          handler.transform intermediate
