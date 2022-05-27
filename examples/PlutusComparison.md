
The purpose of `Contract` in CTL is to bring the capabilities of 
`QueryM` to the public api. 

The current definition of `Contract` in CTL is :

```purescript
type QueryConfig (r :: Row Type) =
  { ogmiosWs :: OgmiosWebSocket
  , datumCacheWs :: DatumCacheWebSocket
  , serverConfig :: ServerConfig
  , wallet :: Maybe Wallet
  -- should probably be more tightly coupled with a wallet
  , usedTxOuts :: UsedTxOuts
  , networkId :: NetworkId
  , slotConfig :: SlotConfig
  , logLevel :: LogLevel
  | r
  }

type DefaultQueryConfig = QueryConfig ()

type QueryM (a :: Type) = ReaderT DefaultQueryConfig (LoggerT Aff) a

type QueryMExtended (r :: Row Type) (a :: Type) = ReaderT (QueryConfig r)
  (LoggerT Aff)
  a
newtype Contract (r :: Row Type) (a :: Type) = Contract (QueryMExtended r a)
```


In CTL we have a general environment type `QueryConfig r`, it stores
the needed parameters to connect to a `Ogmios` server  and
uses the PureScript native row polymorphism to make it extensible by both 
CTL developers and users.

The parameter `a` as in `Plutus` refers to a return value wrapped (in some 
sense). 


Note that in Plutus right now we have the following definition :

```Haskell
type ContractEffs w e =
    '[ Error e
    ,  LogMsg Value
    ,  Writer w
    ,  Checkpoint
    ,  Resumable PABResp PABReq
    ]

type ContractEnv = (IterationID, RequestID)

newtype Contract w (s :: Row *) e a = Contract { unContract :: Eff (ContractEffs w e) a }
  deriving newtype (Functor, Applicative, Monad)
```

The Plutus environment is specialised to just two values and is fixed, it uses
a phantom type to allow users to put more effects on top and provide two parameters
`w` for write and `e` for errors.

As CTL has been writed in PureScript and it's intended to be used inside a 
browser, it makes little sense to allow more general logging capabilities. 
This is the reason why `QueryMExtended` uses directly `LoggerT`. 


