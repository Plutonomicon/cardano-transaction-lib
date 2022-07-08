// Abstracts away unpacking elements from a monomorphic container.
const unpack = obj => {
  const res = [];

  for (let i = 0; i < obj.len(); i++) {
    res.push(obj.get(i));
  }

  return res;
};

const unpackFromProperty = prop => obj => unpack(obj[prop]());

exports._containerHelper = r => ({
  unpack,
  unpackFromProperty,
  // unpacks an associative container where keys are stored in .keys()
  // and values for that keys might be missing.
  unpackKeyIndexed: obj => {
    const res = [];
    for (let i = 0; i < obj.len(); i++) {
      let k = obj.keys().get(i);
      let v = obj.get(k);
      if (v == null) continue;
      res.push(r.tuple(k)(v));
    }
    return res;
  },
  // Abstracts away packing array of something into a monomorphic container.
  pack: (container, elements) => {
    const res = container.new();
    elements.forEach(elem => res.add(elem));
    return res;
  },
  // Abstracts away packing a list of KV-pairs into a map-like structure.
  packMap: (container, entries) => {
    const res = container.new();
    entries.forEach(entry => {
      const [key, value] = r.untuple(entry);
      res.insert(key, value);
    });
    return res;
  },
});
