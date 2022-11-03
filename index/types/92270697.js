// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["92270697"] = [{"values":[{"sourceSpan":{"start":[264,1],"name":".spago/optparse/v4.1.0/src/Options/Applicative/Builder.purs","end":[264,64]},"score":0,"packageInfo":{"values":["optparse"],"tag":"Package"},"name":"completer","moduleName":"Options.Applicative.Builder","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Options","Applicative","Builder","Internal"],"HasCompleter"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Options","Applicative","Types"],"Completer"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Options","Applicative","Builder","Internal"],"Mod"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Add a completer to an argument.\n\nA completer is a function `String -> Effect String` which, given a partial\nargument, returns all possible completions for that argument.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[257,1],"name":".spago/optparse/v4.1.0/src/Options/Applicative/Builder.purs","end":[257,58]},"score":0,"packageInfo":{"values":["optparse"],"tag":"Package"},"name":"action","moduleName":"Options.Applicative.Builder","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Options","Applicative","Builder","Internal"],"HasCompleter"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Options","Applicative","Builder","Internal"],"Mod"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Add a bash completion action. Common actions include `file` and\n`directory`. See\n<http://www.gnu.org/software/bash/manual/html_node/Programmable-Completion-Builtins.html#Programmable-Completion-Builtins>\nfor a complete list.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[202,1],"name":".spago/optparse/v4.1.0/src/Options/Applicative/Builder.purs","end":[202,57]},"score":0,"packageInfo":{"values":["optparse"],"tag":"Package"},"name":"metavar","moduleName":"Options.Applicative.Builder","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Options","Applicative","Builder","Internal"],"HasMetavar"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Options","Applicative","Builder","Internal"],"Mod"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Specify a metavariable for the argument.\n\nMetavariables have no effect on the actual parser, and only serve to specify\nthe symbolic name for an argument to be displayed in the help text.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[174,1],"name":".spago/optparse/v4.1.0/src/Options/Applicative/Builder.purs","end":[174,48]},"score":0,"packageInfo":{"values":["optparse"],"tag":"Package"},"name":"value","moduleName":"Options.Applicative.Builder","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Options","Applicative","Builder","Internal"],"HasValue"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Options","Applicative","Builder","Internal"],"Mod"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Specify a default value for an option.\n\n**Note**: Because this modifier means the parser will never fail,\ndo not use it with combinators such as 'some' or 'many', as\nthese combinators continue until a failure occurs.\nCareless use will thus result in a hang.\n\nTo display the default value, combine with `showDefault` or\n`showDefaultWith`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[186,1],"name":".spago/optparse/v4.1.0/src/Options/Applicative/Builder.purs","end":[186,38]},"score":0,"packageInfo":{"values":["optparse"],"tag":"Package"},"name":"help","moduleName":"Options.Applicative.Builder","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["f",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Options","Applicative","Builder","Internal"],"Mod"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"a"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Specify the help text for an option.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[162,1],"name":".spago/optparse/v4.1.0/src/Options/Applicative/Builder.purs","end":[162,51]},"score":0,"packageInfo":{"values":["optparse"],"tag":"Package"},"name":"long","moduleName":"Options.Applicative.Builder","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Options","Applicative","Builder","Internal"],"HasName"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Options","Applicative","Builder","Internal"],"Mod"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Specify a long name for an option.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[158,1],"name":".spago/optparse/v4.1.0/src/Options/Applicative/Builder.purs","end":[158,50]},"score":0,"packageInfo":{"values":["optparse"],"tag":"Package"},"name":"short","moduleName":"Options.Applicative.Builder","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Options","Applicative","Builder","Internal"],"HasName"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"Char"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Options","Applicative","Builder","Internal"],"Mod"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Specify a short name for an option.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[172,1],"name":".spago/foreign/v6.0.1/src/Foreign.purs","end":[172,54]},"score":9,"packageInfo":{"values":["foreign"],"tag":"Package"},"name":"fail","moduleName":"Foreign","info":{"values":[{"type":{"tag":"ForAll","contents":["m",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad"],"Monad"],"constraintArgs":[{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Foreign"],"ForeignError"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Foreign"],"FT"]},{"tag":"TypeVar","contents":"m"}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Throws a failure error in `FT`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[155,1],"name":".spago/options/v6.0.0/src/Data/Options.purs","end":[155,52]},"score":2,"packageInfo":{"values":["options"],"tag":"Package"},"name":"opt","moduleName":"Data.Options","info":{"values":[{"type":{"tag":"ForAll","contents":["opt",{"tag":"ForAll","contents":["value",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Prim"],"String"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Options"],"Option"]},{"tag":"TypeVar","contents":"opt"}]},{"tag":"TypeVar","contents":"value"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"The default way of creating `Option` values. Constructs an `Option` with\nthe given key, which passes the given value through unchanged.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[68,1],"name":".spago/nonempty/v6.1.0/src/Data/NonEmpty.purs","end":[68,53]},"score":10,"packageInfo":{"values":["nonempty"],"tag":"Package"},"name":"singleton","moduleName":"Data.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Plus"],"Plus"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","NonEmpty"],"NonEmpty"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Create a non-empty structure with a single value.\n\n```purescript\nimport Prelude\n\nsingleton 1 == 1 :| []\nsingleton 1 == 1 :| Nil\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[60,1],"name":".spago/either/v5.0.0/src/Data/Either/Nested.purs","end":[60,31]},"score":32,"packageInfo":{"values":["either"],"tag":"Package"},"name":"in1","moduleName":"Data.Either.Nested","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ForAll","contents":["z",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"BinaryNoParensType","contents":[{"tag":"TypeOp","contents":[["Data","Either","Nested"],"\\/"]},{"tag":"TypeVar","contents":"a"},{"tag":"TypeVar","contents":"z"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[259,1],"name":"src/Internal/Types/TxConstraints.purs","end":[260,71]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"singleton","moduleName":"Ctl.Internal.Types.TxConstraints","info":{"values":[{"type":{"tag":"ForAll","contents":["i",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"ForAll","contents":["o",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","TxConstraints"],"TxConstraint"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","TxConstraints"],"TxConstraints"]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"o"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[265,1],"name":"src/Internal/Types/TxConstraints.purs","end":[266,73]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"mustValidateIn","moduleName":"Ctl.Internal.Types.TxConstraints","info":{"values":[{"type":{"tag":"ForAll","contents":["i",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"ForAll","contents":["o",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","Interval"],"POSIXTimeRange"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","TxConstraints"],"TxConstraints"]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"o"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"`mustValidateIn r` requires the transaction's time range to be contained\n in `r`.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[507,1],"name":"src/Internal/Types/TxConstraints.purs","end":[508,75]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"mustSpendPubKeyOutput","moduleName":"Ctl.Internal.Types.TxConstraints","info":{"values":[{"type":{"tag":"ForAll","contents":["i",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"ForAll","contents":["o",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","Transaction"],"TransactionInput"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","TxConstraints"],"TxConstraints"]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"o"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Spend the given unspent transaction public key output.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[499,1],"name":"src/Internal/Types/TxConstraints.purs","end":[499,79]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"mustSpendAtLeast","moduleName":"Ctl.Internal.Types.TxConstraints","info":{"values":[{"type":{"tag":"ForAll","contents":["i",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"ForAll","contents":["o",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Plutus","Types","Value"],"Value"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","TxConstraints"],"TxConstraints"]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"o"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Requirement to spend inputs with at least the given value\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[280,1],"name":"src/Internal/Types/TxConstraints.purs","end":[281,75]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"mustReferenceOutput","moduleName":"Ctl.Internal.Types.TxConstraints","info":{"values":[{"type":{"tag":"ForAll","contents":["i",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"ForAll","contents":["o",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","Transaction"],"TransactionInput"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","TxConstraints"],"TxConstraints"]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"o"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Require the transaction to reference (not spend!) the given unspent\ntransaction output.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[503,1],"name":"src/Internal/Types/TxConstraints.purs","end":[503,81]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"mustProduceAtLeast","moduleName":"Ctl.Internal.Types.TxConstraints","info":{"values":[{"type":{"tag":"ForAll","contents":["i",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"ForAll","contents":["o",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Plutus","Types","Value"],"Value"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","TxConstraints"],"TxConstraints"]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"o"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Requirement to produce outputs with at least the given value\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[420,1],"name":"src/Internal/Types/TxConstraints.purs","end":[420,76]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"mustMintValue","moduleName":"Ctl.Internal.Types.TxConstraints","info":{"values":[{"type":{"tag":"ForAll","contents":["i",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"ForAll","contents":["o",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Plutus","Types","Value"],"Value"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","TxConstraints"],"TxConstraints"]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"o"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Mint the given `Value`\nThe amount to mint must not be zero.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[275,1],"name":"src/Internal/Types/TxConstraints.purs","end":[275,79]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"mustIncludeDatum","moduleName":"Ctl.Internal.Types.TxConstraints","info":{"values":[{"type":{"tag":"ForAll","contents":["i",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"ForAll","contents":["o",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","Datum"],"Datum"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","TxConstraints"],"TxConstraints"]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"o"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Require the transaction to include a datum.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[270,1],"name":"src/Internal/Types/TxConstraints.purs","end":[271,76]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"mustBeSignedBy","moduleName":"Ctl.Internal.Types.TxConstraints","info":{"values":[{"type":{"tag":"ForAll","contents":["i",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"ForAll","contents":["o",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","PubKeyHash"],"PaymentPubKeyHash"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Ctl","Internal","Types","TxConstraints"],"TxConstraints"]},{"tag":"TypeVar","contents":"i"}]},{"tag":"TypeVar","contents":"o"}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Require the transaction to be signed by the public key.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[111,1],"name":".spago/transformers/v5.2.0/src/Control/Monad/List/Trans.purs","end":[111,57]},"score":19,"packageInfo":{"values":["transformers"],"tag":"Package"},"name":"singleton","moduleName":"Control.Monad.List.Trans","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Applicative"],"Applicative"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","List","Trans"],"ListT"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Create a list with one element.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[140,1],"name":".spago/transformers/v5.2.0/src/Control/Monad/List/Trans.purs","end":[140,48]},"score":19,"packageInfo":{"values":["transformers"],"tag":"Package"},"name":"repeat","moduleName":"Control.Monad.List.Trans","info":{"values":[{"type":{"tag":"ForAll","contents":["f",{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Control","Monad"],"Monad"],"constraintArgs":[{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Control","Monad","List","Trans"],"ListT"]},{"tag":"TypeVar","contents":"f"}]},{"tag":"TypeVar","contents":"a"}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Generate an infinite list by repeating a value.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[269,1],"name":"src/Contract/Transaction.purs","end":[273,42]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"signTransaction","moduleName":"Contract.Transaction","info":{"values":[{"type":{"tag":"ForAll","contents":["tx",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"ForAll","contents":["r",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Row"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Newtype"],"Newtype"],"constraintArgs":[{"tag":"TypeVar","contents":"tx"},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","Transaction"],"Transaction"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"tx"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Contract","Monad"],"Contract"]},{"tag":"TypeVar","contents":"r"}]},{"tag":"TypeConstructor","contents":[["Contract","Transaction"],"BalancedSignedTransaction"]}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Signs a transaction with potential failure.\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[565,1],"name":"src/Contract/Transaction.purs","end":[569,24]},"score":0,"packageInfo":{"values":[],"tag":"LocalPackage"},"name":"createAdditionalUtxos","moduleName":"Contract.Transaction","info":{"values":[{"type":{"tag":"ForAll","contents":["tx",{"tag":"TypeConstructor","contents":[["Prim"],"Type"]},{"tag":"ForAll","contents":["r",{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Row"]},{"tag":"TypeConstructor","contents":[["Prim"],"Type"]}]},{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Newtype"],"Newtype"],"constraintArgs":[{"tag":"TypeVar","contents":"tx"},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Cardano","Types","Transaction"],"Transaction"]}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"tx"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Contract","Monad"],"Contract"]},{"tag":"TypeVar","contents":"r"}]},{"tag":"TypeConstructor","contents":[["Ctl","Internal","Plutus","Types","Transaction"],"UtxoMap"]}]}]}]},null]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Builds an expected utxo set from transaction outputs. Predicts output\nreferences (`TransactionInput`s) for each output by calculating the\ntransaction hash and indexing the outputs in the order they appear in the\ntransaction. This function should be used for transaction chaining\nin conjunction with `mustUseAdditionalUtxos` balancer constraint.\nThrows an exception if conversion to Plutus outputs fails.\n"}],"tag":"SearchResult"}]