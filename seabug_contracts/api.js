
const CallContract = import("./CallContract.purs");

/**
 * TODO
 */
exports.callMarketPlaceBuy = async (str) => {
    const CC = await CallContract;
    return CC.callMarketPlaceBuyTest({ name: str })();
};

/**
 * TODO
 */
exports.callMarketPlaceListNft = async (str) => {
    const CC = await CallContract;
    return CC.callMarketPlaceBuyTest({ name: str })();
};
