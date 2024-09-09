// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["279496439"] = [{"values":[{"sourceSpan":{"start":[80,1],"name":".spago/untagged-union/v1.0.0/src/Untagged/Union.purs","end":[80,69]},"score":0,"packageInfo":{"values":["untagged-union"],"tag":"Package"},"name":"toEither1","moduleName":"Untagged.Union","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["b",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Untagged","TypeCheck"],"HasRuntimeType"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Untagged","Union"],"OneOf"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Either"],"Either"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Unwraps a single layer of `OneOf` to an Either\nNote that for some `x :: a |+| b`. If the value `x` has a runtime\nvalue that can be read as either types `a` and `b`, then\n`toEither1 x` will return `Left`.\n\nExample: toEither1 (asOneOf 3.0 :: Int |+| Number) == Left 3\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[176,1],"name":"test/CoinSelection/RoundRobin.purs","end":[182,29]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"runMockRoundRobin","moduleName":"Test.Ctl.CoinSelection.RoundRobin","info":{"values":[{"type":{"tag":"ForAll","contents":["k",{"tag":"ForAll","contents":["n",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"k"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"n"}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","EuclideanRing"],"EuclideanRing"],"constraintArgs":[{"tag":"TypeVar","contents":"n"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Test","Ctl","CoinSelection","RoundRobin"],"MockRoundRobinState"]},{"tag":"TypeVar","contents":"k"}]},{"tag":"TypeVar","contents":"n"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Test","Ctl","CoinSelection","RoundRobin"],"MockRoundRobinState"]},{"tag":"TypeVar","contents":"k"}]},{"tag":"TypeVar","contents":"n"}]}]}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[148,1],"name":".spago/parsing/v10.2.0/src/Parsing/Indent.purs","end":[148,60]},"score":1,"packageInfo":{"values":["parsing"],"tag":"Package"},"name":"withPos","moduleName":"Parsing.Indent","info":{"values":[{"type":{"tag":"ForAll","contents":["s",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Parsing","Indent"],"IndentParser"]},{"tag":"TypeVar","contents":"s"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Parsing","Indent"],"IndentParser"]},{"tag":"TypeVar","contents":"s"}]},{"tag":"TypeVar","contents":"a"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Parses using the current location for indentation reference\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[134,1],"name":".spago/tuples/v7.0.0/src/Data/Tuple.purs","end":[134,43]},"score":39,"packageInfo":{"values":["tuples"],"tag":"Package"},"name":"swap","moduleName":"Data.Tuple","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["b",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple"],"Tuple"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple"],"Tuple"]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeVar","contents":"a"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Exchange the first and second components of a tuple.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[179,1],"name":".spago/these/v6.0.0/src/Data/These.purs","end":[179,43]},"score":1,"packageInfo":{"values":["these"],"tag":"Package"},"name":"swap","moduleName":"Data.These","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["b",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","These"],"These"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"b"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","These"],"These"]},{"tag":"TypeVar","contents":"b"}]},{"tag":"TypeVar","contents":"a"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Swap between `This` and `That`, and flips the order for `Both`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[498,1],"name":".spago/variant/v8.0.0/src/Data/Functor/Variant.purs","end":[498,51]},"score":2,"packageInfo":{"values":["variant"],"tag":"Package"},"name":"revariantF","moduleName":"Data.Functor.Variant","info":{"values":[{"type":{"tag":"ForAll","contents":["r",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Functor","Variant"],"UnvariantF"]},{"tag":"TypeVar","contents":"r"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Functor","Variant"],"VariantF"]},{"tag":"TypeVar","contents":"r"}]},{"tag":"TypeVar","contents":"a"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Reconstructs a VariantF given an UnvariantF eliminator.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[472,1],"name":".spago/variant/v8.0.0/src/Data/Functor/Variant.purs","end":[475,19]},"score":2,"packageInfo":{"values":["variant"],"tag":"Package"},"name":"unvariantF","moduleName":"Data.Functor.Variant","info":{"values":[{"type":{"tag":"ForAll","contents":["r",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Functor","Variant"],"VariantF"]},{"tag":"TypeVar","contents":"r"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Functor","Variant"],"UnvariantF"]},{"tag":"TypeVar","contents":"r"}]},{"tag":"TypeVar","contents":"a"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"A low-level eliminator which reifies the `IsSymbol`, `Cons` and\n`Functor` constraints require to reconstruct the Variant. This\nlets you work generically with some VariantF at runtime.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[61,1],"name":".spago/arrays/v7.1.0/src/Data/Array/ST/Iterator.purs","end":[61,54]},"score":21,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"exhausted","moduleName":"Data.Array.ST.Iterator","info":{"values":[{"type":{"tag":"ForAll","contents":["r",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","ST","Iterator"],"Iterator"]},{"tag":"TypeVar","contents":"r"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","ST","Internal"],"ST"]},{"tag":"TypeVar","contents":"r"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Check whether an iterator has been exhausted.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[31,1],"name":".spago/transformers/v6.0.0/src/Control/Monad/Writer.purs","end":[31,49]},"score":24,"packageInfo":{"values":["transformers"],"tag":"Package"},"name":"runWriter","moduleName":"Control.Monad.Writer","info":{"values":[{"type":{"tag":"ForAll","contents":["w",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","Writer"],"Writer"]},{"tag":"TypeVar","contents":"w"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple"],"Tuple"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"w"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Run a computation in the `Writer` monad\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[27,1],"name":".spago/transformers/v6.0.0/src/Control/Monad/Writer.purs","end":[27,46]},"score":24,"packageInfo":{"values":["transformers"],"tag":"Package"},"name":"writer","moduleName":"Control.Monad.Writer","info":{"values":[{"type":{"tag":"ForAll","contents":["w",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple"],"Tuple"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeVar","contents":"w"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","Writer"],"Writer"]},{"tag":"TypeVar","contents":"w"}]},{"tag":"TypeVar","contents":"a"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Creates a `Writer` from a result and output pair.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[120,1],"name":".spago/st/v6.2.0/src/Control/Monad/ST/Internal.purs","end":[120,55]},"score":7,"packageInfo":{"values":["st"],"tag":"Package"},"name":"read","moduleName":"Control.Monad.ST.Internal","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["r",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","ST","Internal"],"STRef"]},{"tag":"TypeVar","contents":"r"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","ST","Internal"],"ST"]},{"tag":"TypeVar","contents":"r"}]},{"tag":"TypeVar","contents":"a"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Read the current value of a mutable reference.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[38,1],"name":".spago/transformers/v6.0.0/src/Control/Monad/Except.purs","end":[38,50]},"score":24,"packageInfo":{"values":["transformers"],"tag":"Package"},"name":"runExcept","moduleName":"Control.Monad.Except","info":{"values":[{"type":{"tag":"ForAll","contents":["e",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","Except"],"Except"]},{"tag":"TypeVar","contents":"e"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Either"],"Either"]},{"tag":"TypeVar","contents":"e"}]},{"tag":"TypeVar","contents":"a"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Run a computation in the `Except` monad. The inverse of `except`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[27,1],"name":".spago/transformers/v6.0.0/src/Control/Comonad/Env.purs","end":[27,43]},"score":24,"packageInfo":{"values":["transformers"],"tag":"Package"},"name":"runEnv","moduleName":"Control.Comonad.Env","info":{"values":[{"type":{"tag":"ForAll","contents":["e",{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Comonad","Env"],"Env"]},{"tag":"TypeVar","contents":"e"}]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Tuple"],"Tuple"]},{"tag":"TypeVar","contents":"e"}]},{"tag":"TypeVar","contents":"a"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Unwrap a value in the `Env` comonad.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[265,1],"name":".spago/plutus-types/v1.0.1/src/Cardano/Plutus/Types/Map.purs","end":[265,49]},"score":0,"packageInfo":{"values":["plutus-types"],"tag":"Package"},"name":"fromCardano","moduleName":"Cardano.Plutus.Types.Map","info":{"values":[{"type":{"tag":"ForAll","contents":["k",{"tag":"ForAll","contents":["v",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Map","Internal"],"Map"]},{"tag":"TypeVar","contents":"k"}]},{"tag":"TypeVar","contents":"v"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Cardano","Plutus","Types","Map"],"Map"]},{"tag":"TypeVar","contents":"k"}]},{"tag":"TypeVar","contents":"v"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"}]