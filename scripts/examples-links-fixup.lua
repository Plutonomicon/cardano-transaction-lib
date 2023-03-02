revision = pandoc.pipe("git", {"rev-parse", "HEAD"}, "")

function Link(el)
  el.target = string.gsub(el.target, "../examples/", "https://github.com/Plutonomicon/cardano-transaction-lib/blob/" .. revision .. "/examples/")
  el.target = string.gsub(el.target, "../src/", "https://github.com/Plutonomicon/cardano-transaction-lib/blob/" .. revision .. "/src/")
  el.target = string.gsub(el.target, "../test/", "https://github.com/Plutonomicon/cardano-transaction-lib/blob/" .. revision .. "/test/")
  return el
end
