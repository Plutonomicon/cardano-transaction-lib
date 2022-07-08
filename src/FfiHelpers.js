/* global exports */

// Abstracts away unpacking elements from a monomorphic container.
const unpack = (obj) => {
  const res = [];

  for (let i = 0; i < obj.len(); i++) {
    res.push(obj.get(i));
  }

  return res;
};

// unpacks an associative container where keys are stored in .keys()
// and values for that keys might be missing.
const unpackKeyIndexed = (tuple) => (obj) => {
  const res = [];
  for (let i = 0; i < obj.len(); i++) {
    var k = obj.keys().get(i);
    var v = obj.get(k);
    if (v == null) continue;
    res.push(tuple(k)(v));
  }
  return res;
};

exports._containerHelper = (r) => ({
  // Abstracts away packing array of something into a monomorphic container.
  pack: (container, elements) => {
    const res = container.new();
    elements.forEach((elem) => res.add(elem));
    return res;
  },
  // Abstracts away packing a list of KV-pairs into a map-like structure.
  packMap: (container, entries) => {
    const res = container.new();
    entries.forEach((entry) => {
      const [key, value] = r.untuple(entry);
      res.insert(key, value);
    });
    return res;
  },
  unpack,
  unpackKeyIndexed: unpackKeyIndexed(r.tuple),
  unpackFromProperty: (prop) => (obj) => unpack(obj[prop]()),
});
