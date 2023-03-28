// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["968357784"] = [{"values":[{"sourceSpan":{"start":[379,1],"name":"src/Contract/Transaction.purs","end":[384,16]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"withBalancedTxWithConstraints","moduleName":"Contract.Transaction","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","ProcessConstraints","UnbalancedTx"],"UnbalancedTx"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","BalanceTx","Constraints"],"BalanceTxConstraintsBuilder"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","BalanceTx","Types"],"FinalizedTransaction"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","Contract","Monad"],"Contract"]},{"tag":"TypeVar","contents":"a"}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","Contract","Monad"],"Contract"]},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Execute an action on a balanced transaction (`balanceTx` will\nbe called). Within this function, all transaction inputs\nused by this transaction will be locked, so that they are not\nused in any other context.\nAfter the function completes, the locks will be removed.\nErrors will be thrown.\n"}],"tag":"SearchResult"}]