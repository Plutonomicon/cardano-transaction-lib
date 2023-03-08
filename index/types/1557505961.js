// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["1557505961"] = [{"values":[{"sourceSpan":{"start":[96,1],"name":".spago/typelevel/v6.0.0/src/Data/Typelevel/Num/Aliases.purs","end":[96,29]},"score":1,"packageInfo":{"values":["typelevel"],"tag":"Package"},"name":"D100","moduleName":"Data.Typelevel.Num.Aliases","info":{"values":[{"type":{"tag":"BinaryNoParensType","contents":[{"tag":"TypeOp","contents":[["Data","Typelevel","Num","Reps"],":*"]},{"tag":"ParensInType","contents":{"tag":"BinaryNoParensType","contents":[{"tag":"TypeOp","contents":[["Data","Typelevel","Num","Reps"],":*"]},{"tag":"TypeConstructor","contents":[["Data","Typelevel","Num","Reps"],"D1"]},{"tag":"TypeConstructor","contents":[["Data","Typelevel","Num","Reps"],"D0"]}]}},{"tag":"TypeConstructor","contents":[["Data","Typelevel","Num","Reps"],"D0"]}]},"arguments":[]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[268,1],"name":"src/Internal/Service/Blockfrost.purs","end":[270,4]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"BlockfrostServiceM","moduleName":"Ctl.Internal.Service.Blockfrost","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","Logger","Trans"],"LoggerT"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","Reader","Trans"],"ReaderT"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Service","Blockfrost"],"BlockfrostServiceParams"]}]},{"tag":"TypeConstructor","contents":[["Effect","Aff"],"Aff"]}]}}]},{"tag":"TypeVar","contents":"a"}]},"arguments":[["a",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[653,1],"name":"src/Internal/QueryM.purs","end":[653,78]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"SubmitTxListenerSet","moduleName":"Ctl.Internal.QueryM","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","QueryM"],"ListenerSet"]},{"tag":"ParensInType","contents":{"tag":"BinaryNoParensType","contents":[{"tag":"TypeOp","contents":[["Data","Tuple","Nested"],"/\\"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","QueryM","Ogmios"],"TxHash"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","CborBytes"],"CborBytes"]}]}}]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","QueryM","Ogmios"],"SubmitTxR"]}]},"arguments":[]}],"tag":"TypeSynonymResult"},"hashAnchor":"t","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[287,1],"name":"src/Internal/QueryM/Ogmios.purs","end":[287,60]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"submitTxCall","moduleName":"Ctl.Internal.QueryM.Ogmios","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","QueryM","JsonWsp"],"JsonWspCall"]},{"tag":"ParensInType","contents":{"tag":"BinaryNoParensType","contents":[{"tag":"TypeOp","contents":[["Data","Tuple","Nested"],"/\\"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","QueryM","Ogmios"],"TxHash"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","CborBytes"],"CborBytes"]}]}}]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","QueryM","Ogmios"],"SubmitTxR"]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Sends a serialized signed transaction with its full witness through the\nCardano network via Ogmios.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[295,1],"name":"src/Internal/QueryM/Ogmios.purs","end":[295,77]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"evaluateTxCall","moduleName":"Ctl.Internal.QueryM.Ogmios","info":{"values":[{"type":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","QueryM","JsonWsp"],"JsonWspCall"]},{"tag":"ParensInType","contents":{"tag":"BinaryNoParensType","contents":[{"tag":"TypeOp","contents":[["Data","Tuple","Nested"],"/\\"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","CborBytes"],"CborBytes"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","QueryM","Ogmios"],"AdditionalUtxoSet"]}]}}]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","QueryM","Ogmios"],"TxEvaluationR"]}]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Evaluates the execution units of scripts present in a given transaction,\nwithout actually submitting the transaction.\n"}],"tag":"SearchResult"}]