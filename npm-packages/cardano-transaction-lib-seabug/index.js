const CallContract = import("./dist/bundle.js");

/**
 * Exists temporarily for testing purposes. Returns given argument.
 */
exports.callMarketPlaceBuyTest = async (str) => {
    const CC = await CallContract;
    return CC.callMarketPlaceBuyTest(str)();
};

/**
 * Calls Seabug Contract 'marketPlaceBuy'.
 * It returns a promise holding no data.
 *
 * First argument 'config' should be an object adhering
 * to structure:
 *
 *   config ::
 *   { serverHost :: String             - CTL Haskell Server host address
 *   , serverPort :: Int                - CTL Haskell Server port
 *   , serverSecureConn :: Boolean      - If the server connection uses secure communication
 *   , ogmiosHost :: String             - Ogmios service host adddress
 *   , ogmiosPort :: Int                - Ogmios service port
 *   , ogmiosSecureConn :: Boolean      - If the Ogmios connection uses secure communication
 *   , datumCacheHost :: String         - Datum Cache service host address
 *   , datumCachePort :: Int            - Datum Cache service port
 *   , datumCacheSecureConn :: Boolean  - If the Ogmios connection uses secure communication
 *   , networkId :: Int                 - On which Cardano network should contracts be run
 *   , projectId :: String              - blockfrost.io API key
 *   }
 *
 * Second argument 'args' should be an object with a structure described below:
 *
 *   { nftCollectionArgs ::
 *       { collectionNftCs :: String      - CurrencySymbol of nft collection
 *       , lockLockup :: BigInt           -
 *       , lockLockupEnd :: BigInt        -
 *       , lockingScript :: String        - ValidatorHash of a script locking the nft
 *       , author :: String               - PaymentPubKeyHash of the nft author
 *       , daoScript :: String            - ValidatorHash
 *       , authorShare :: BigInt          -
 *       , daoShare :: BigInt             - Natural
 *       }
 *   , nftIdArgs ::
 *       { collectionNftTn :: String      - TokenName of the nft collection
 *       , price :: BigInt                - Natural
 *       , owner :: String                - PaymentPubKeyHash of the nft current owner
 *       }
 *   }
 *
 */
exports.callMarketPlaceBuy = async (config, args) => {
    const CC = await CallContract;
    return CC.callMarketPlaceBuy(config)(args)();
};

/**
 * Calls Seabug Contract 'marketPlaceListNft'.
 * Returns a promise holding no nft listings.
 *
 * First argument 'config' should be an object adhering
 * to structure:
 *
 *   config ::
 *   { server_host :: String               - CTL Haskell Server host address
 *   , server_port :: Int                  - CTL Haskell Server port
 *   , server_secure_conn :: Boolean       - If the server connection uses secure communication
 *   , ogmios_host :: String               - Ogmios service host adddress
 *   , ogmios_port :: Int                  - Ogmios service port
 *   , ogmios_secure_conn :: Boolean       - If the Ogmios connection uses secure communication
 *   , datum_cache_host :: String          - Datum Cache service host address
 *   , datum_cache_port :: Int             - Datum Cache service port
 *   , datum_cache_secure_conn :: Boolean  - If the Ogmios connection uses secure communication
 *   , networkId :: Int                    - On which Cardano network should contracts be run
 *   }
 *
 * The output is an array of objects described below:
 *
 *    { input :: { transaction_id :: String, input_index :: Int }
 *    , output :: { address :: String, value :: ValueOut, data_hash :: String }
 *    , metadata ::
 *        { seabugMetadata ::
 *            { policyId :: String --MintingPolicyHash
 *            , mintPolicy :: String --ByteArray
 *            , collectionNftCS :: String -- CurrencySymbol
 *            , collectionNftTN :: String -- TokenName
 *            , lockingScript :: String --ValidatorHash
 *            , authorPkh :: String -- PubKeyHash
 *            , authorShare :: BigInt -- Share
 *            , marketplaceScript :: String -- ValidatorHash
 *            , marketplaceShare :: BigInt -- share
 *            , ownerPkh :: String -- PubKeyHash
 *            , ownerPrice :: BigInt --Natural
 *            }
 *        , ipfsHash :: String
 *        }
 *    }
 *
 *  where
 *
 *    ValueOut is an array of object described by:
 *
 *      { currencySymbol :: String, tokenName :: String, amount :: BigInt }
 *
 */
exports.callMarketPlaceListNft = async (config) => {
    const CC = await CallContract;
    return CC.callMarketPlaceListNft(config)();
};
