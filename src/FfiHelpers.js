/* global exports */

// Abstracts away unpacking elements from a monomorphic container.
const unpack = obj => {
    const res = [];

    for (let i = 0; i < obj.len(); i++) {
        res.push(obj.get(i));
    };

    return res;
};

exports.containerHelper = {
    // Abstracts away packing array of something into a monomorphic container.
    pack: (container, elements) => {
        const res = container.new();
        elements.forEach(elem => res.add(elem));
        return res;
    },
    unpack,
    unpackFromProperty: prop => obj => unpack(obj[prop]())
};
