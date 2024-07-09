// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["1267484798"] = [{"values":[{"sourceSpan":{"start":[466,1],"name":"src/Internal/Plutip/Server.purs","end":[469,57]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"startPlutipCluster","moduleName":"Ctl.Internal.Plutip.Server","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Plutip","Types"],"PlutipConfig"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Test","UtxoDistribution"],"InitialUTxODistribution"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","Aff"],"Aff"]},{"tag":"ParensInType","contents":{"tag":"BinaryNoParensType","contents":[{"tag":"TypeOp","contents":[["Data","Tuple","Nested"],"/\\"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Wallet","Key"],"PrivatePaymentKey"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Plutip","Types"],"ClusterStartupParameters"]}]}}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Start the plutip cluster, initializing the state with the given\nUTxO distribution. Also initializes an extra payment key (aka\n`ourKey`) with some UTxOs for use with further plutip\nsetup. `ourKey` has funds proportional to the total amount of the\nUTxOs in the passed distribution, so it can be used to handle\ntransaction fees.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[110,1],"name":"src/Internal/BalanceTx/ExUnitsAndMinFee.purs","end":[113,38]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"evalExUnitsAndMinFee","moduleName":"Ctl.Internal.BalanceTx.ExUnitsAndMinFee","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","BalanceTx","UnattachedTx"],"IndexedTx"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Cardano","Types","UtxoMap"],"UtxoMap"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","BalanceTx","Types"],"BalanceTxM"]},{"tag":"ParensInType","contents":{"tag":"BinaryNoParensType","contents":[{"tag":"TypeOp","contents":[["Data","Tuple","Nested"],"/\\"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","BalanceTx","UnattachedTx"],"EvaluatedTx"]},{"tag":"TypeConstructor","contents":[["Cardano","Types","Coin"],"Coin"]}]}}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[58,1],"name":"src/Contract/UnbalancedTx.purs","end":[61,56]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"mkUnbalancedTxE","moduleName":"Contract.UnbalancedTx","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","ScriptLookups"],"ScriptLookups"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","TxConstraints"],"TxConstraints"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","Contract","Monad"],"Contract"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Either"],"Either"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","ProcessConstraints","Error"],"MkUnbalancedTxError"]}]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","ProcessConstraints","UnbalancedTx"],"UnbalancedTx"]}]}}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Create an `UnbalancedTx` given `ScriptLookups` and\n`TxConstraints`. This should be called in conjuction with\n`balanceTx` and  `signTransaction`.\n\nThis is a 'non-throwing' variant; if you need the 'throwing' variant, use\n`mkUnbalancedTx` instead.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[311,1],"name":"src/Contract/Transaction.purs","end":[314,65]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"balanceTxWithConstraintsE","moduleName":"Contract.Transaction","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","ProcessConstraints","UnbalancedTx"],"UnbalancedTx"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","BalanceTx","Constraints"],"BalanceTxConstraintsBuilder"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","Contract","Monad"],"Contract"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Either"],"Either"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","BalanceTx","Error"],"BalanceTxError"]}]},{"tag":"TypeConstructor","contents":[["Cardano","Types","Transaction"],"Transaction"]}]}}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Attempts to balance an `UnbalancedTx` using the specified\nbalancer constraints.\n\n`balanceTxWithConstraints` is a throwing variant.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[164,1],"name":".spago/affjax/v13.0.0/src/Affjax.purs","end":[164,58]},"score":0,"packageInfo":{"values":["affjax"],"tag":"Package"},"name":"delete_","moduleName":"Affjax","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Affjax"],"AffjaxDriver"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Affjax"],"URL"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Effect","Aff"],"Aff"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Either"],"Either"]},{"tag":"TypeConstructor","contents":[["Affjax"],"Error"]}]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}}]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Makes a `DELETE` request to the specified URL and ignores the response\nbody.\n"}],"tag":"SearchResult"}]