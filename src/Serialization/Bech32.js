const Bech32 = require("bech32");

exports.verifyBech32Impl_ = maybe => limit => str => {
    try{
        var ret = Bech32.decode(str, limit);
        if (ret != null){
            return maybe.just(str);
        }
    }
    catch(e){
        console.log(e);
    }
    return maybe.nothing;
};

// only safe on valid bech32 strings
exports.getPrefixImpl_ = bech32str => limit => {
    return Bech32.decode(bech32str, limit).prefix;
};
