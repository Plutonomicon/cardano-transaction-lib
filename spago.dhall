{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies = 
  [ "aff"
  , "arraybuffer-types"
  , "arrays"
  , "argonaut"
  , "bigints"
  , "console"
  , "const"
  , "control"
  , "debug"
  , "either"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "identity"
  , "maybe"
  , "medea"
  , "mote"
  , "node-buffer"
  , "node-fs-aff"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "rationals"
  , "refs"
  , "spec"
  , "strings"
  , "transformers" 
  , "tuples"
  , "uint"
  , "undefined"
  , "unordered-collections"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs"
  , "test/**/*.purs"
  ]
}
