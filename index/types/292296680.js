// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["292296680"] = [{"values":[{"sourceSpan":{"start":[364,1],"name":"src/Contract/Transaction.purs","end":[368,16]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"withBalancedTxs","moduleName":"Contract.Transaction","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","ProcessConstraints","UnbalancedTx"],"UnbalancedTx"]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","BalanceTx","Types"],"FinalizedTransaction"]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","Contract","Monad"],"Contract"]},{"tag":"TypeVar","contents":"a"}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","Contract","Monad"],"Contract"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Same as `withBalancedTxsWithConstraints`, but uses the default balancer\nconstraints.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[538,1],"name":"src/Contract/Test/Assert.purs","end":[542,21]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"checkLossAtAddress","moduleName":"Contract.Test.Assert","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Contract","Test","Assert"],"Labeled"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Plutus","Types","Address"],"Address"]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","Contract","Monad"],"Contract"]},{"tag":"TypeConstructor","contents":[["Data","BigInt"],"BigInt"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Contract","Test","Assert"],"ContractCheck"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Requires that the computed amount of lovelace was lost at the address\nby calling the contract.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[518,1],"name":"src/Contract/Test/Assert.purs","end":[522,21]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"checkGainAtAddress","moduleName":"Contract.Test.Assert","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Contract","Test","Assert"],"Labeled"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Plutus","Types","Address"],"Address"]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","Contract","Monad"],"Contract"]},{"tag":"TypeConstructor","contents":[["Data","BigInt"],"BigInt"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Contract","Test","Assert"],"ContractCheck"]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Requires that the computed amount of lovelace was gained at the address\nby calling the contract.\n"}],"tag":"SearchResult"}]