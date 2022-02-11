/* global exports */

exports.containerHelper = {
    // Abstracts away packing array of something into a monomorphic container.
    pack: (container, elements) => {
        const res = container.new();
        elements.forEach(res.add);
        return res;
    }
};
