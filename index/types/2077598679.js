// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["2077598679"] = [{"values":[{"sourceSpan":{"start":[37,1],"name":".spago/checked-exceptions/v3.1.1/src/Control/Monad/Except/Checked.purs","end":[45,23]},"score":0,"packageInfo":{"values":["checked-exceptions"],"tag":"Package"},"name":"handleError","moduleName":"Control.Monad.Except.Checked","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["handlers",{"tag":"ForAll","contents":["excHandled",{"tag":"ForAll","contents":["excIn",{"tag":"ForAll","contents":["excOut",{"tag":"ForAll","contents":["rl",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Prim","RowList"],"RowToList"],"constraintArgs":[{"tag":"TypeVar","contents":"handlers"},{"tag":"TypeVar","contents":"rl"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Variant","Internal"],"VariantMatchCases"],"constraintArgs":[{"tag":"TypeVar","contents":"rl"},{"tag":"TypeVar","contents":"excHandled"},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","Except","Checked"],"ExceptV"]},{"tag":"TypeVar","contents":"excOut"}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Prim","Row"],"Union"],"constraintArgs":[{"tag":"TypeVar","contents":"excHandled"},{"tag":"TypeVar","contents":"excOut"},{"tag":"TypeVar","contents":"excIn"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad"],"Monad"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Record"]},{"tag":"TypeVar","contents":"handlers"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","Except","Checked"],"ExceptV"]},{"tag":"TypeVar","contents":"excIn"}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","Except","Checked"],"ExceptV"]},{"tag":"TypeVar","contents":"excOut"}]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]}]}]}]}]}]},null]},null]},null]},null]},null]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Catches and eliminates exceptions given a record of handlers.\nUnhandled exceptions are re-propragated. Record fields map\nto the label for the exception being handled.\n\nAn example for handling HTTP exceptions might be:\n```purescript\nrequest # handleError\n  { httpNotFound: \\_ -> ...\n  , httpServerError: \\error -> ...\n  }\n```\n"}],"tag":"SearchResult"}]