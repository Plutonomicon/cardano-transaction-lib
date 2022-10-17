// This file was generated by purescript-docs-search
window.DocsSearchTypeIndex["230943629"] = [{"values":[{"sourceSpan":{"start":[638,1],"name":".spago/lists/v6.0.1/src/Data/List.purs","end":[638,92]},"score":17,"packageInfo":{"values":["lists"],"tag":"Package"},"name":"groupAllBy","moduleName":"Data.List","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"List"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"List"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"NonEmptyList"]},{"tag":"TypeVar","contents":"a"}]}}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Group equal elements of a list into lists, using the specified\nequivalence relation to determine equality.\n\nFor example,\n\n```purescript\ngroupAllBy (\\a b -> odd a && odd b) (1 : 3 : 2 : 4 : 3 : 3 : Nil) ==\n   (NonEmptyList (NonEmpty 1 Nil)) : (NonEmptyList (NonEmpty 2 Nil)) : (NonEmptyList (NonEmpty 3 (3 : 3 : Nil))) : (NonEmptyList (NonEmpty 4 Nil)) : Nil\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[624,1],"name":".spago/lists/v6.0.1/src/Data/List.purs","end":[624,80]},"score":17,"packageInfo":{"values":["lists"],"tag":"Package"},"name":"groupBy","moduleName":"Data.List","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"List"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"List"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"NonEmptyList"]},{"tag":"TypeVar","contents":"a"}]}}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Group equal, consecutive elements of a list into lists, using the specified\nequivalence relation to determine equality.\n\nFor example,\n\n```purescript\ngroupBy (\\a b -> odd a && odd b) (1 : 3 : 2 : 4 : 3 : 3 : Nil) ==\n  (NonEmptyList (NonEmpty 1 (3 : Nil))) : (NonEmptyList (NonEmpty 2 Nil)) : (NonEmptyList (NonEmpty 4 Nil)) : (NonEmptyList (NonEmpty 3 (3 : Nil))) : Nil\n```\n\nRunning time: `O(n)`\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[277,1],"name":".spago/lists/v6.0.1/src/Data/List/NonEmpty.purs","end":[277,104]},"score":17,"packageInfo":{"values":["lists"],"tag":"Package"},"name":"groupAllBy","moduleName":"Data.List.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"ConstrainedType","contents":[{"constraintClass":[["Data","Ord"],"Ord"],"constraintArgs":[{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"NonEmptyList"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"NonEmptyList"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"NonEmptyList"]},{"tag":"TypeVar","contents":"a"}]}}]}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[274,1],"name":".spago/lists/v6.0.1/src/Data/List/NonEmpty.purs","end":[274,92]},"score":17,"packageInfo":{"values":["lists"],"tag":"Package"},"name":"groupBy","moduleName":"Data.List.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"NonEmptyList"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"NonEmptyList"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Types"],"NonEmptyList"]},{"tag":"TypeVar","contents":"a"}]}}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":null}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[573,1],"name":".spago/lists/v6.0.1/src/Data/List/Lazy.purs","end":[573,80]},"score":17,"packageInfo":{"values":["lists"],"tag":"Package"},"name":"groupBy","moduleName":"Data.List.Lazy","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Lazy","Types"],"List"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Lazy","Types"],"List"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","List","Lazy","Types"],"NonEmptyList"]},{"tag":"TypeVar","contents":"a"}]}}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Group equal, consecutive elements of a list into lists, using the specified\nequivalence relation to determine equality.\n\nRunning time: `O(n)`\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[1019,1],"name":".spago/arrays/v6.0.1/src/Data/Array.purs","end":[1019,83]},"score":17,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"groupAllBy","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Data","Ordering"],"Ordering"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Group equal elements of an array into arrays, using the specified\ncomparison function to determine equality.\n\n```purescript\ngroupAllBy (comparing Down) [1, 3, 2, 4, 3, 3]\n   = [NonEmptyArray [4], NonEmptyArray [3, 3, 3], NonEmptyArray [2], NonEmptyArray [1]]\n```\n\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[998,1],"name":".spago/arrays/v6.0.1/src/Data/Array.purs","end":[998,79]},"score":17,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"groupBy","moduleName":"Data.Array","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Array"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Group equal, consecutive elements of an array into arrays, using the\nspecified equivalence relation to determine equality.\n\n```purescript\ngroupBy (\\a b -> odd a && odd b) [1, 3, 2, 4, 3, 3]\n   = [NonEmptyArray [1, 3], NonEmptyArray [2], NonEmptyArray [4], NonEmptyArray [3, 3]]\n```\n\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[449,1],"name":".spago/arrays/v6.0.1/src/Data/Array/NonEmpty.purs","end":[449,99]},"score":17,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"groupAllBy","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Data","Ordering"],"Ordering"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Group equal elements of an array into arrays, using the specified\ncomparison function to determine equality.\n\n```purescript\ngroupAllBy (comparing Down) (NonEmptyArray [1, 3, 2, 4, 3, 3])\n   = NonEmptyArray [NonEmptyArray [4], NonEmptyArray [3, 3, 3], NonEmptyArray [2], NonEmptyArray [1]]\n```\n"}],"tag":"SearchResult"},{"values":[{"sourceSpan":{"start":[439,1],"name":".spago/arrays/v6.0.1/src/Data/Array/NonEmpty.purs","end":[439,95]},"score":17,"packageInfo":{"values":["arrays"],"tag":"Package"},"name":"groupBy","moduleName":"Data.Array.NonEmpty","info":{"values":[{"type":{"tag":"ForAll","contents":["a",{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeVar","contents":"a"}]},{"tag":"TypeConstructor","contents":[["Prim"],"Boolean"]}]}]}}]},{"tag":"TypeApp","contents":[{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Prim"],"Function"]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}]},{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"ParensInType","contents":{"tag":"TypeApp","contents":[{"tag":"TypeConstructor","contents":[["Data","Array","NonEmpty","Internal"],"NonEmptyArray"]},{"tag":"TypeVar","contents":"a"}]}}]}]}]},null]}}],"tag":"ValueResult"},"hashAnchor":"v","comments":"Group equal, consecutive elements of an array into arrays, using the\nspecified equivalence relation to determine equality.\n\n```purescript\ngroupBy (\\a b -> odd a && odd b) (NonEmptyArray [1, 3, 2, 4, 3, 3])\n   = NonEmptyArray [NonEmptyArray [1, 3], NonEmptyArray [2], NonEmptyArray [4], NonEmptyArray [3, 3]]\n```\n\n"}],"tag":"SearchResult"}]