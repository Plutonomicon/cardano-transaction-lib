// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["1926948317"] = [{"values":[{"sourceSpan":{"start":[235,1],"name":".spago/optparse/v4.1.0/src/Options/Applicative/Builder.purs","end":[235,67]},"score":0,"packageInfo":{"values":["optparse"],"tag":"Package"},"name":"command","moduleName":"Options.Applicative.Builder","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Options","Applicative","Types"],"ParserInfo"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Options","Applicative","Builder","Internal"],"Mod"]},{"tag":"TypeConstructor","contents":[["Options","Applicative","Builder","Internal"],"CommandFields"]}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Add a command to a subparser option.\n\nSuggested usage for multiple commands is to add them to a single subparser. e.g.\n\n```purescript\nsample :: Parser Sample\nsample = subparser\n       ( command \"hello\"\n         (info hello (progDesc \"Print greeting\"))\n      <> command \"goodbye\"\n         (info goodbye (progDesc \"Say goodbye\"))\n       )\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[17,1],"name":".spago/encoding/v0.0.7/src/Data/TextDecoder.purs","end":[17,64]},"score":0,"packageInfo":{"values":["encoding"],"tag":"Package"},"name":"decode","moduleName":"Data.TextDecoder","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Data","TextDecoder"],"Encoding"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","ArrayBuffer","Types"],"ArrayView"]},{"tag":"TypeVar","contents":"a"}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Either"],"Either"]},{"tag":"TypeConstructor","contents":[["Effect","Exception"],"Error"]}]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Decodes an `ArrayBufferView` with the given `Encoding`.\nReturns an `Error` if decoding fails.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[176,1],"name":"src/Internal/Plutip/UtxoDistribution.purs","end":[180,21]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"transferFundsFromEnterpriseToBase","moduleName":"Ctl.Internal.Plutip.UtxoDistribution","info":{"values":[{"type":{"tag":"ForAll","contents":["r",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Row"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Wallet","Key"],"PrivatePaymentKey"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Wallet","Key"],"KeyWallet"]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Contract","Monad"],"Contract"]},{"tag":"TypeVar","contents":"r"}]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"For each wallet which includes a stake key, transfer the value of\nthe utxos at its enterprise address to its base address. Note\nthat this function clears the `usedTxOuts` cache, so it should\nnot be used if there could be items in the cache that shouldn't\nbe cleared (this function is intended to be used only on plutip\nstartup).\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[569,1],"name":"src/Contract/Test/Utils.purs","end":[573,26]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"assertOutputHasRefScript","moduleName":"Contract.Test.Utils","info":{"values":[{"type":{"tag":"ForAll","contents":["r",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Row"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","ScriptRef"],"ScriptRef"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Contract","Test","Utils"],"Labeled"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Plutus","Types","Transaction"],"TransactionOutputWithRefScript"]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Contract","Test","Utils"],"ContractTestM"]},{"tag":"TypeVar","contents":"r"}]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Requires that the transaction output contains the specified reference\nscript.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[537,1],"name":"src/Contract/Test/Utils.purs","end":[541,26]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"assertOutputHasDatum","moduleName":"Contract.Test.Utils","info":{"values":[{"type":{"tag":"ForAll","contents":["r",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Row"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","OutputDatum"],"OutputDatum"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Contract","Test","Utils"],"Labeled"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Plutus","Types","Transaction"],"TransactionOutputWithRefScript"]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Contract","Test","Utils"],"ContractTestM"]},{"tag":"TypeVar","contents":"r"}]},{"tag":"TypeConstructor","contents":[["Data","Unit"],"Unit"]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Requires that the transaction output contains the specified datum or\ndatum hash.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[598,3],"name":".spago/aeson/bfd8f4dcd0522a076320f9dc710c24817438e02e/src/Aeson.purs","end":[598,67]},"score":0,"packageInfo":{"values":["aeson"],"tag":"Package"},"name":"tupleFromArray","moduleName":"Aeson","info":{"values":[{"typeClassArguments":[["a",null]],"typeClass":[["Aeson"],"DecodeTupleAux"],"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Aeson"],"DecodeTupleAux"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Int"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeConstructor","contents":[["Aeson"],"Aeson"]}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Either"],"Either"]},{"tag":"TypeConstructor","contents":[["Data","Argonaut","Decode","Error"],"JsonDecodeError"]}]},{"tag":"TypeVar","contents":"a"}]}]}]}]},null]}}],"tag":"TypeClassMemberResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]