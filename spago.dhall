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
  , "control"
  , "debug"
  , "either"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "generics-rep"
  , "identity"
  , "int-53"
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
  , "uint"
  , "unordered-collections"
  , "undefined"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
