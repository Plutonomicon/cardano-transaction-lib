// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["1469852600"] = [{"values":[{"sourceSpan":{"start":[429,1],"name":"src/Internal/QueryM.purs","end":[433,11]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"withQueryRuntime","moduleName":"Ctl.Internal.QueryM","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","QueryM"],"QueryConfig"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","QueryM"],"QueryRuntime"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","Aff"],"Aff"]},{"tag":"TypeVar","contents":"a"}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","Aff"],"Aff"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Constructs and finalizes a contract environment that is usable inside a\nbracket callback.\nMake sure that `Aff` action does not end before all contracts that use the\nruntime terminate. Otherwise `WebSocket`s will be closed too early.\n"}],"tag":"SearchResult"}]