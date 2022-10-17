// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["2063697772"] = [{"values":[{"sourceSpan":{"start":[87,1],"name":".spago/variant/v7.0.3/src/Data/Variant/Internal.purs","end":[87,45]},"score":2,"packageInfo":{"values":["variant"],"tag":"Package"},"name":"lookupTag","moduleName":"Data.Variant.Internal","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"List"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"A specialized lookup function which bails early. Foldable's `elem`\nis always worst-case.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[96,1],"name":".spago/strings/v5.0.0/src/Data/String/Common.purs","end":[96,60]},"score":17,"packageInfo":{"values":["strings"],"tag":"Package"},"name":"joinWith","moduleName":"Data.String.Common","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Joins the strings in the array together, inserting the first argument\nas separator between them.\n\n```purescript\njoinWith \", \" [\"apple\", \"banana\", \"orange\"] == \"apple, banana, orange\"\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[64,1],"name":"src/Internal/Wallet.purs","end":[64,68]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"mkKeyWallet","moduleName":"Ctl.Internal.Wallet","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Wallet","Key"],"PrivatePaymentKey"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Wallet","Key"],"PrivateStakeKey"]}]}]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Wallet"],"Wallet"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[77,1],"name":"src/Internal/Wallet/Key.purs","end":[78,61]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"privateKeysToKeyWallet","moduleName":"Ctl.Internal.Wallet.Key","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Wallet","Key"],"PrivatePaymentKey"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Wallet","Key"],"PrivateStakeKey"]}]}]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Wallet","Key"],"KeyWallet"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[216,1],"name":"src/Internal/Serialization/WitnessSet.purs","end":[216,78]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"_mkRedeemers","moduleName":"Ctl.Internal.Serialization.WitnessSet","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","FfiHelpers"],"ContainerHelper"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Serialization","Types"],"Redeemer"]}]}]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Serialization","Types"],"Redeemers"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[127,1],"name":"src/Internal/Plutus/Types/Address.purs","end":[127,75]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"pubKeyHashAddress","moduleName":"Ctl.Internal.Plutus.Types.Address","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","PubKeyHash"],"PaymentPubKeyHash"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","PubKeyHash"],"StakePubKeyHash"]}]}]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Plutus","Types","Address"],"Address"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"The address that should be targeted by a transaction output locked\nby the public key with the given hash.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[77,1],"name":"src/Internal/BalanceTx/Constraints.purs","end":[78,70]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"mustBalanceTxWithAddresses","moduleName":"Ctl.Internal.BalanceTx.Constraints","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Serialization","Address"],"NetworkId"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Plutus","Types","Address"],"Address"]}]}]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","BalanceTx","Constraints"],"BalanceTxConstraintsBuilder"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Tells the balancer to treat the provided addresses like user's own.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[75,1],"name":"src/Contract/Wallet.purs","end":[76,58]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"mkKeyWalletFromPrivateKeys","moduleName":"Contract.Wallet","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Wallet","Key"],"PrivatePaymentKey"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Maybe"],"Maybe"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Wallet","Key"],"PrivateStakeKey"]}]}]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Wallet"],"Wallet"]}]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]