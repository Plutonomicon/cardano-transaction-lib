exports._to_str = x => {
    return x.to_str();
};
exports._vrfKeyHashShow = byteArrayToHex => vrfKeyhash =>
    byteArrayToHex(vrfKeyhash.to_bytes());
