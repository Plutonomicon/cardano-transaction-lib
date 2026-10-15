module Ctl.Internal.Contract.Provider
  ( providerForCtlBackend
  , providerForBlockfrostBackend
  , providerForSelfHostedBlockfrostBackend
  ) where

import Cardano.Blockfrost.BlockfrostBackend (BlockfrostBackend)
import Cardano.Blockfrost.Provider (providerForBlockfrostBackend) as Blockfrost
import Cardano.Blockfrost.Service (BlockfrostServiceM, runBlockfrostServiceM)
import Cardano.Kupmios.KupmiosM (KupmiosM)
import Cardano.Kupmios.Provider as Kupmios
import Cardano.Provider.Type (Provider)
import Ctl.Internal.Contract.LogParams (LogParams)
import Ctl.Internal.Contract.ProviderBackend (CtlBackend)
import Ctl.Internal.Helpers (logWithLevel)
import Data.Maybe (fromMaybe)
import Effect.Aff (Aff)

providerForCtlBackend
  :: forall rest
   . (forall (a :: Type). LogParams rest -> CtlBackend -> KupmiosM a -> Aff a)
  -> LogParams rest
  -> CtlBackend
  -> Provider
providerForCtlBackend runKupmiosM params backend =
  Kupmios.providerForKupmiosBackend runKupmiosM'
  where
  runKupmiosM' :: forall (a :: Type). KupmiosM a -> Aff a
  runKupmiosM' = runKupmiosM params backend

providerForBlockfrostBackend
  :: forall rest. LogParams rest -> BlockfrostBackend -> Provider
providerForBlockfrostBackend logParams backend =
  Blockfrost.providerForBlockfrostBackend runBlockfrostServiceM'
  where
  runBlockfrostServiceM' :: forall (a :: Type). BlockfrostServiceM a -> Aff a
  runBlockfrostServiceM' = runBlockfrostServiceM
    (fromMaybe logWithLevel logParams.customLogger logParams.logLevel)
    backend

providerForSelfHostedBlockfrostBackend
  :: forall rest
   . LogParams rest
  -> BlockfrostBackend
  -> (forall (a :: Type). LogParams rest -> CtlBackend -> KupmiosM a -> Aff a)
  -> CtlBackend
  -> Provider
providerForSelfHostedBlockfrostBackend
  params
  blockfrostBackend
  runKupmiosM
  ctlBackend =
  let
    blockfrostProvider = providerForBlockfrostBackend params
      blockfrostBackend
    ctlProvider = providerForCtlBackend runKupmiosM params ctlBackend
  in
    blockfrostProvider
      { evaluateTx = ctlProvider.evaluateTx
      , submitTx = ctlProvider.submitTx
      }
