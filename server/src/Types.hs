module Types (
  AppM (AppM),
  ServerOptions (..),
  Env (..),
  Cbor (..),
  Fee (..),
  ApplyArgsRequest (..),
  AppliedScript (..),
  HashScriptRequest (..),
  HashedScript (..),
  FeeEstimateError (..),
  CardanoBrowserServerError (..),
  hashLedgerScript,
  newEnvIO,
  unsafeDecode,
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as Shelley
import Cardano.Binary qualified as Cbor
import Codec.Serialise (serialise)
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Aeson (FromJSON, ToJSON (toJSON))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Aeson.Encoding
import Data.Aeson.Types (withText)
import Data.Bifunctor (second)
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.ByteString.Short qualified as SBS
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (Port)
import Paths_cardano_browser_tx_server (getDataFileName)
import Plutus.V1.Ledger.Api qualified as Ledger
import Plutus.V1.Ledger.Scripts qualified as Ledger.Scripts
import Servant (FromHttpApiData, QueryParam', Required, ToHttpApiData)
import Servant.Docs qualified as Docs
import Text.Read (readMaybe)
import Utils (tshow)

newtype AppM (a :: Type) = AppM (ReaderT Env IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Env
    , MonadThrow
    )

newtype Env = Env
  { protocolParams :: Shelley.ProtocolParameters
  }
  deriving stock (Generic)

newtype ServerOptions = ServerOptions
  { port :: Port
  }
  deriving stock (Generic)

newEnvIO :: IO (Either String Env)
newEnvIO =
  getDataFileName "config/pparams.json"
    >>= Aeson.eitherDecodeFileStrict @Shelley.ProtocolParameters
    <&> second Env

newtype Cbor = Cbor Text
  deriving stock (Show)
  deriving newtype (Eq, FromHttpApiData, ToHttpApiData)

newtype Fee = Fee Integer
  deriving stock (Show, Generic)
  deriving newtype (Eq)

instance ToJSON Fee where
  -- to avoid issues with integer parsing in PS, we should probably return
  -- a JSON string, and not a number
  toJSON (Fee int) = Aeson.String $ tshow int

  toEncoding (Fee int) = Aeson.Encoding.integerText int

instance FromJSON Fee where
  parseJSON =
    withText "Fee" $
      maybe (fail "Expected quoted integer") (pure . Fee)
        . readMaybe @Integer
        . Text.unpack

data ApplyArgsRequest = ApplyArgsRequest
  { script :: Ledger.Script
  , args :: [Ledger.Data]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype AppliedScript = AppliedScript Ledger.Script
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON, ToJSON)

-- This is only to avoid an orphan instance for @ToDocs@
newtype HashScriptRequest = HashScriptRequest Ledger.Script
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON, ToJSON)

-- This is a newtype to avoid the orphan instance
newtype HashedScript = HashedScript Ledger.Scripts.ScriptHash
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON, ToJSON)

-- Adapted from `plutus-apps` implementation
hashLedgerScript :: Ledger.Script -> Ledger.Scripts.ScriptHash
hashLedgerScript =
  Ledger.Scripts.ScriptHash
    . Ledger.toBuiltin
    . C.serialiseToRawBytes
    . C.hashScript
    . toCardanoApiScript

-- Adapted from `plutus-apps` implementation
toCardanoApiScript :: Ledger.Script -> C.Script C.PlutusScriptV1
toCardanoApiScript =
  C.PlutusScript C.PlutusScriptV1
    . Shelley.PlutusScriptSerialised
    . SBS.toShort
    . LC8.toStrict
    . serialise

-- We'll probably extend this with more error types over time
newtype CardanoBrowserServerError = FeeEstimate FeeEstimateError
  deriving stock (Show)

instance Exception CardanoBrowserServerError

data FeeEstimateError
  = InvalidCbor Cbor.DecoderError
  | InvalidHex String
  deriving stock (Show)

instance Exception FeeEstimateError

-- API doc stuff
instance Docs.ToParam (QueryParam' '[Required] "tx" Cbor) where
  toParam _ =
    Docs.DocQueryParam
      "tx"
      [sampleTx]
      "A CBOR-encoded `Tx AlonzoEra`; should be sent as a hexadecimal string"
      Docs.Normal
    where
      sampleTx =
        mconcat
          [ "84a300818258205d677265fa5bb21ce6d8c7502aca70b93"
          , "16d10e958611f3c6b758f65ad9599960001818258390030"
          , "fb3b8539951e26f034910a5a37f22cb99d94d1d409f69dd"
          , "baea9711c12f03c1ef2e935acc35ec2e6f96c650fd3bfba"
          , "3e96550504d5336100021a0002b569a0f5f6"
          ]

instance Docs.ToSample Fee where
  toSamples _ =
    [
      ( "The `Fee` will be returned encoded as a JSON string"
      , Fee 160265
      )
    ]

instance Docs.ToSample ApplyArgsRequest where
  toSamples _ =
    [
      ( "Both the `script` and each of its `args` should be hex-encoded CBOR"
      , exampleRequest
      )
    ]
    where
      exampleRequest :: ApplyArgsRequest
      exampleRequest =
        ApplyArgsRequest
          { script = exampleScript
          , args = [unsafeDecode "Data" "\"01\""]
          }

instance Docs.ToSample AppliedScript where
  toSamples _ =
    [
      ( "The applied script will be returned as hex-encoded CBOR"
      , AppliedScript exampleScript
      )
    ]

instance Docs.ToSample HashScriptRequest where
  toSamples _ = [] -- TODO

instance Docs.ToSample HashedScript where
  toSamples _ = [] -- TODO

-- For decoding test fixtures, samples, etc...
unsafeDecode :: forall (a :: Type). FromJSON a => String -> LC8.ByteString -> a
unsafeDecode name = fromMaybe (error errorMsg) . Aeson.decode
  where
    errorMsg :: String
    errorMsg = "Failed to decode `" <> name <> "`"

-- TODO
-- Replace this with a simpler script
exampleScript :: Ledger.Script
exampleScript =
  unsafeDecode
    "Script"
    $ mconcat
      [ "\""
      , "590a9a01000032333222323232323233223233223232333222333222333222"
      , "33223233322232323322323233223233333222223322333332222233223322"
      , "33223232323232222222223235300f00222353013002222232222222333353"
      , "02901023232323232300100e3200135505a2253353503e0011533530593335"
      , "303d12001051500332635302e3357389210b756e726561636861626c650002"
      , "f01f150051500422135355054002225335305d333573466e3c009406417c17"
      , "854cd4c174ccd4c10448004155401c00454024540204c01800c4cd40f0cd54"
      , "140c0d4010cdc0a40009001281e8a99a982a99ab9c4901124e4654206d7573"
      , "74206265206275726e6564000561500110561533530543301b00f353035002"
      , "2220011500115335305433573892011f4f776e6572206d757374207369676e"
      , "20746865207472616e73616374696f6e000551500110551533530533335530"
      , "1c120013502f50482353022001222533530573303f00333041304901a50431"
      , "0581333573466e1cccc07000806cd4c0e001488800d200205905800b105513"
      , "357389211f556e6465726c79696e67204e4654206d75737420626520756e6c"
      , "6f636b656400054232323232323225335305a33300f00835303b0082220020"
      , "011500215335305a33573892013e45786163746c79206f6e65206e65772074"
      , "6f6b656e206d757374206265206d696e74656420616e642065786163746c79"
      , "206f6e65206f6c64206275726e740005b15002105b15335305833004500233"
      , "042304a018504415335305833004500133042304b01a504415335305833355"
      , "30211200135034504d3300533042304b35303900622200150443370266e054"
      , "00cc1514004c151400804041685415854158541584cdc199b8250020184828"
      , "270044cdc199b8250010154828270044d4c0d800c888008894cd4c158ccd5c"
      , "d19b880020530580571058133355301f1200135032504b3300300100200e22"
      , "23530240012225335305933041006003153353059333573466e1c014ccc078"
      , "0080e40e416c16854cd4d4110004854cd4d4114d4c09805488888888894cd4"
      , "d413cccd54c0b44800540b08d4d54178004894cd4c19cccd5cd19b8f00200e"
      , "0690681350540031505300221350523535505e001220011505021323335734"
      , "66ebc008004178174c8d4d5415400488cdd2a400066ae80dd480119aba0375"
      , "20026ec4090cd54155405cc0e8024416c4168416841688c894cd4c154ccc02"
      , "800c004d4c0d800c8880045400854cd4c154cd5ce2493e45786163746c7920"
      , "6f6e65206e657720746f6b656e206d757374206265206d696e74656420616e"
      , "642065786163746c79206f6e65206f6c64206275726e740005615002105615"
      , "33530533301a00e353034001222001105513357389211f4f776e6572206d75"
      , "7374207369676e20746865207472616e73616374696f6e0005423232323230"
      , "0100d320013550592253353503d001153353503d32635302d3357389210b75"
      , "6e726561636861626c650002e01e150042213300500200122135355053002"
      , "225335305c333573466e3c009406017817454cd4d410400454020884cc0240"
      , "080044c01800c88d4d54140008894cd4d40f800c54cd4c164ccd5cd19b8f00"
      , "2303800705b05a153353059333573466e1c005200205b05a15006150051500"
      , "5221500715335305433573892011e45786163746c79206f6e65204e4654206"
      , "d757374206265206d696e7465640005515001105515335305333355301c120"
      , "013502f50482353022001222533530573303f00333041304901a5043133357"
      , "3466e1cccc07000806cd4c0e001488800d2002059058105800b10551335738"
      , "9211d556e6465726c79696e67204e4654206d757374206265206c6f636b656"
      , "4000542322232323001007320013550532253353503700110532213535504d"
      , "0022253353056333573466e3c009404816015c54cd4d40ec004415c884d4d5"
      , "4144008894cd4d40fc00c416c884d4d5415400888c94cd4d411001054cd4c1"
      , "7cccd5cd19b8f007501306106015335305f333573466e3c00d404018418054"
      , "cd4c17cccd5cd19b87006480041841804ccd5cd19b87002480081841804180"
      , "540045400488418854cd4c178ccd5cd19b8f002501206005f15335305e3335"
      , "73466e3c019403c18017c54cd4c178ccd5cd19b870014800418017c4ccd5cd"
      , "19b870054800818017c417c417c417c4c01800c4c0b8d4c0c0010888ccc0d0"
      , "00c0140104c0ac0044d4c03800488cccd4c0580048c98d4c070cd5ce249024"
      , "c680001d00d2001232635301c3357389201024c680001d00d232635301c335"
      , "7389201024c680001d00d22232323001005320013550422233535026001480"
      , "0088d4d540f0008894cd4c114ccd5cd19b8f00200904704613007001130060"
      , "033200135504122335350250014800088d4d540ec008894cd4c110ccd5cd19"
      , "b8f00200704604510011300600349888d4c01c00888888888894cd4d40c0cc"
      , "d54c03848005403494cd4c118ccd5cd19b8f00c00104804713503300115032"
      , "00321048104613350162253353502500221003100150243200135503a22112"
      , "22533535021001135350190032200122133353501b00522002300400233355"
      , "30071200100500400122123300100300220012222222222123333333333001"
      , "00b00a00900800700600500400300220012221233300100400300220012122"
      , "22300400521222230030052122223002005212222300100520011200120012"
      , "12222300400522122223300300600522122223300200600521222230010052"
      , "001123350032233353501d00322002002001353501b0012200112212330010"
      , "030021200123724666aa600a24002e28008cc88cc008c8dc9198120008029a"
      , "9802801911001198011b923530050032220013300237246a600a0064440060"
      , "02a010a0129110022212333001004003002200132001355020221122253353"
      , "5007001100222133005002333553007120010050040013200135501f221222"
      , "53353500600215335350060011023221024221533535008003102422153353"
      , "02533007004002133353009120010070030011026112200212212233001004"
      , "00312001223530030022235300500322323353010005233530110042533530"
      , "21333573466e3c00800408c0885400c408880888cd4c044010808894cd4c08"
      , "4ccd5cd19b8f00200102302215003102215335350090032153353500a00221"
      , "335300e0022335300f00223353013002233530140022330180020012025233"
      , "53014002202523301800200122202522233530110042025222533530263335"
      , "73466e1c01800c0a009c54cd4c098ccd5cd19b870050020280271330210040"
      , "01102710271020153353500900121020102022123300100300220011212230"
      , "02003112200112001212230020032221223330010050040032001212230020"
      , "0321223001003200122333573466e3c00800404003c4cd4008894cd4c03400"
      , "8403c400403048848cc00400c0084800488d4d5400c00888d4d5401400c894"
      , "cd4c038ccd5cd19b8f00400201000f133009003001100f1122123300100300"
      , "211200122333573466e1c00800402402094cd4c014ccd5cd19b88001002007"
      , "0061480004005208092f401133573892113526f79616c6974696573206e6f7"
      , "42070616964000033200135500422253353004333573466e20009208004006"
      , "00513371600400226600666e0c0092080043371666e1800920800400112200"
      , "21220012001112323001001223300330020020011"
      , "\""
      ]
