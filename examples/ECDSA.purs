module Ctl.Examples.ECDSA (contract) where

import Contract.Prelude

import Cardano.Transaction.Builder
  ( DatumWitness(DatumValue)
  , OutputWitness(PlutusScriptOutput)
  , ScriptWitness(ScriptValue)
  , TransactionBuilderStep(SpendOutput, Pay)
  )
import Cardano.Types
  ( OutputDatum(OutputDatumHash)
  , TransactionOutput(TransactionOutput)
  )
import Cardano.Types.Credential (Credential(ScriptHashCredential))
import Cardano.Types.DataHash (hashPlutusData)
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.Transaction as Transaction
import Cardano.Types.TransactionUnspentOutput (_input, fromUtxoMap, toUtxoMap)
import Contract.Address (mkAddress)
import Contract.Crypto.Secp256k1.ECDSA
  ( ECDSAPublicKey
  , ECDSASignature
  , MessageHash
  , deriveEcdsaSecp256k1PublicKey
  , signEcdsaSecp256k1
  , unECDSAPublicKey
  , unMessageHash
  )
import Contract.Crypto.Secp256k1.Utils
  ( hashMessageSha256
  , randomSecp256k1PrivateKey
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class ToData
  , PlutusData(Constr)
  , RedeemerDatum(RedeemerDatum)
  , toData
  )
import Contract.Prim.ByteArray (byteArrayFromIntArrayUnsafe)
import Contract.Scripts (Validator, validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , submitTxFromBuildPlan
  )
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Data.Array as Array
import Data.Lens (view)
import Data.Map as Map
import Noble.Secp256k1.ECDSA (unECDSASignature)

newtype ECDSARedemeer = ECDSARedemeer
  { msg :: MessageHash
  , sig :: ECDSASignature
  , pk :: ECDSAPublicKey
  }

derive instance Generic ECDSARedemeer _
derive instance Newtype ECDSARedemeer _

instance ToData ECDSARedemeer where
  toData (ECDSARedemeer { msg, sig, pk }) = Constr BigNum.zero
    [ toData $ unMessageHash msg
    , toData $ unECDSASignature sig
    , toData $ unECDSAPublicKey pk
    ]

contract :: Contract Unit
contract = do
  void $ prepTest >>= testECDSA

-- | Prepare the ECDSA test by locking some funds at the validator address
prepTest :: Contract TransactionHash
prepTest = do
  validator <- liftContractM "Cannot get validator" getValidator
  let
    valHash = validatorHash validator
    val = Value.lovelaceValueOf BigNum.one
  scriptAddress <- mkAddress
    (wrap $ ScriptHashCredential valHash)
    Nothing
  tx <- submitTxFromBuildPlan Map.empty mempty
    [ Pay $ TransactionOutput
        { address: scriptAddress
        , amount: val
        , datum: Just $ OutputDatumHash $ hashPlutusData PlutusData.unit
        , scriptRef: Nothing
        }
    ]
  let txId = Transaction.hash tx
  logInfo' $ "Submitted ECDSA test preparation tx: " <> show txId
  awaitTxConfirmed txId
  logInfo' $ "Transaction confirmed: " <> show txId
  pure txId

-- | Attempt to unlock one utxo using an ECDSA signature
testVerification
  :: TransactionHash -> ECDSARedemeer -> Contract TransactionHash
testVerification txId ecdsaRed = do
  let redeemer = RedeemerDatum $ toData ecdsaRed

  validator <- liftContractM "Can't get validator" getValidator
  let valHash = validatorHash validator

  valAddr <- mkAddress (wrap $ ScriptHashCredential valHash) Nothing

  scriptUtxos <- utxosAt valAddr
  utxo <- liftContractM "No UTxOs found at validator address"
    $ Array.head
    $ Array.filter (view _input >>> unwrap >>> _.transactionId >>> eq txId)
    $ fromUtxoMap scriptUtxos

  tx <- submitTxFromBuildPlan (toUtxoMap [ utxo ]) mempty
    [ SpendOutput utxo $ Just
        $ PlutusScriptOutput
            (ScriptValue validator)
            redeemer
        $ Just
        $ DatumValue
        $ PlutusData.unit
    ]
  let txId' = Transaction.hash tx
  logInfo' $ "Submitted ECDSA test verification tx: " <> show txId'
  awaitTxConfirmed txId'
  logInfo' $ "Transaction confirmed: " <> show txId'
  pure txId'

-- | Testing ECDSA verification function on-chain
testECDSA :: TransactionHash -> Contract TransactionHash
testECDSA txId = do
  privateKey <- liftEffect $ randomSecp256k1PrivateKey
  let
    publicKey = deriveEcdsaSecp256k1PublicKey privateKey
    message = byteArrayFromIntArrayUnsafe [ 0, 1, 2, 3 ]
  messageHash <- liftAff $ hashMessageSha256 message
  signature <- liftAff $ signEcdsaSecp256k1 privateKey messageHash
  testVerification txId $
    ECDSARedemeer
      { msg: messageHash
      , sig: signature
      , pk: publicKey
      }

getValidator :: Maybe Validator
getValidator =
  decodeTextEnvelope validateECDSA >>= plutusScriptFromEnvelope

validateECDSA :: String
validateECDSA =
  """
{
    "type": "PlutusScriptV2",
    "description": "",
    "cborHex": "59080459080101000032323232332232323232323232323233223232323232223232322323253353232323500222253353335734666ed000400c00808007c40804cd5ce249167369676e617475726520636865636b206661696c65640001f3333573466e1cd55cea80224000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd406806cd5d0a80619a80d00d9aba1500b33501a01c35742a014666aa03ceb94074d5d0a804999aa80f3ae501d35742a01066a03404a6ae85401cccd54078099d69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40c1d69aba150023031357426ae8940088c98c80d4cd5ce01b01a81989aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a8183ad35742a00460626ae84d5d1280111931901a99ab9c036035033135573ca00226ea8004d5d09aba2500223263203133573806406205e26aae7940044dd50009aba1500533501a75c6ae854010ccd540780888004d5d0a801999aa80f3ae200135742a00460486ae84d5d1280111931901699ab9c02e02d02b135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00860286ae84d5d1280211931900f99ab9c02001f01d3333573466e1cd55cea803a40004666444246660020080060046eb8d5d0a8039bae35742a00c6eb8d5d09aba2500623263201e33573803e03c0386666ae68cdc39aab9d37540109000100e91931900e99ab9c01e01d01b101c13263201c3357389201035054350001c135573ca00226ea80044d5d1280089aab9e5001137540022464460046eb0004c8004d5405888cccd55cf80092805119a80498021aba100230033574400402c464646666ae68cdc39aab9d5002480008cc8848cc00400c008c030d5d0a80118029aba135744a004464c6402c66ae7005c0580504d55cf280089baa0012323232323333573466e1cd55cea8022400046666444424666600200a0080060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008c054d5d0a80119a80780a1aba135744a004464c6403666ae7007006c0644d55cf280089baa00135742a008666aa010eb9401cd5d0a8019919191999ab9a3370ea0029002119091118010021aba135573ca00646666ae68cdc3a80124004464244460020086eb8d5d09aab9e500423333573466e1d400d20002122200323263201d33573803c03a03603403226aae7540044dd50009aba1500233500b75c6ae84d5d1280111931900b99ab9c018017015135744a00226ae8940044d55cf280089baa0011335500175ceb44488c88c008dd5800990009aa80991191999aab9f00225008233500733221233001003002300635573aa004600a6aae794008c010d5d100180a09aba100111220021221223300100400312232323333573466e1d4005200023212230020033005357426aae79400c8cccd5cd19b8750024800884880048c98c8048cd5ce00980900800789aab9d500113754002464646666ae68cdc3a800a400c46424444600800a600e6ae84d55cf280191999ab9a3370ea004900211909111180100298049aba135573ca00846666ae68cdc3a801a400446424444600200a600e6ae84d55cf280291999ab9a3370ea00890001190911118018029bae357426aae7940188c98c8048cd5ce00980900800780700689aab9d500113754002464646666ae68cdc39aab9d5002480008cc8848cc00400c008c014d5d0a8011bad357426ae8940088c98c8038cd5ce00780700609aab9e5001137540024646666ae68cdc39aab9d5001480008dd71aba135573ca004464c6401866ae700340300284dd5000919191919191999ab9a3370ea002900610911111100191999ab9a3370ea004900510911111100211999ab9a3370ea00690041199109111111198008048041bae35742a00a6eb4d5d09aba2500523333573466e1d40112006233221222222233002009008375c6ae85401cdd71aba135744a00e46666ae68cdc3a802a400846644244444446600c01201060186ae854024dd71aba135744a01246666ae68cdc3a8032400446424444444600e010601a6ae84d55cf280591999ab9a3370ea00e900011909111111180280418071aba135573ca018464c6402a66ae7005805404c04804404003c0380344d55cea80209aab9e5003135573ca00426aae7940044dd50009191919191999ab9a3370ea002900111999110911998008028020019bad35742a0086eb4d5d0a8019bad357426ae89400c8cccd5cd19b875002480008c8488c00800cc020d5d09aab9e500623263200e33573801e01c01801626aae75400c4d5d1280089aab9e500113754002464646666ae68cdc3a800a400446424460020066eb8d5d09aab9e500323333573466e1d400920002321223002003375c6ae84d55cf280211931900599ab9c00c00b009008135573aa00226ea8004488c8c8cccd5cd19b87500148010848880048cccd5cd19b875002480088c84888c00c010c018d5d09aab9e500423333573466e1d400d20002122200223263200c33573801a01801401201026aae7540044dd50009191999ab9a3370ea0029001100291999ab9a3370ea0049000100291931900419ab9c009008006005135573a6ea800448800848800526120014910350543100112323001001223300330020020011"
}
"""
