
const CallContract = import("./CallContract.purs");

/**
 * TODO
 */
exports.callMarketPlaceBuy = async (config, args) => {
    const CC = await CallContract;
    return CC.callMarketPlaceBuy(config)(args)();
};

/**
 * TODO
 */
exports.callMarketPlaceListNft = async (config) => {
    const CC = await CallContract;
    return CC.callMarketPlaceListNft(config)();
};
