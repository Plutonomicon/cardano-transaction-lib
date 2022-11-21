/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

const setter = prop => obj => value => () => obj["set_" + prop](value);

exports.newAuxiliaryData = () => lib.AuxiliaryData.new();

exports._hashAuxiliaryData = auxiliaryData =>
  lib.hash_auxiliary_data(auxiliaryData).to_bytes();

exports.setAuxiliaryDataNativeScripts = setter("native_scripts");

exports.setAuxiliaryDataPlutusScripts = setter("plutus_scripts");

exports.setAuxiliaryDataGeneralTransactionMetadata = setter("metadata");

exports.newGeneralTransactionMetadata = containerHelper => entries => () =>
  containerHelper.packMap(lib.GeneralTransactionMetadata, entries);

exports.newMetadataMap = containerHelper => entries => () =>
  lib.TransactionMetadatum.new_map(
    containerHelper.packMap(lib.MetadataMap, entries)
  );

exports.newMetadataList = containerHelper => entries => () =>
  lib.TransactionMetadatum.new_list(
    containerHelper.pack(lib.MetadataList, entries)
  );

exports.newMetadataInt = int => () => lib.TransactionMetadatum.new_int(int);

exports.newMetadataBytes = bytes => () =>
  lib.TransactionMetadatum.new_bytes(bytes);

exports.newMetadataText = text => () => lib.TransactionMetadatum.new_text(text);
