{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20230105/packages.dhall
        sha256:3e9fbc9ba03e9a1fcfd895f65e2d50ee2f5e86c4cd273f3d5c841b655a0e1bda

let additions =
      { aeson =
        { dependencies =
          [ "aff"
          , "argonaut"
          , "argonaut-codecs"
          , "argonaut-core"
          , "arrays"
          , "bifunctors"
          , "const"
          , "control"
          , "effect"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "integers"
          , "js-bigints"
          , "lists"
          , "maybe"
          , "mote"
          , "numbers"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "quickcheck"
          , "record"
          , "spec"
          , "strings"
          , "tuples"
          , "typelevel"
          , "typelevel-prelude"
          , "uint"
          , "untagged-union"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-aeson.git"
        , version = "v2.0.1"
        }
      , bignumber =
        { dependencies =
          [ "console"
          , "effect"
          , "either"
          , "exceptions"
          , "functions"
          , "integers"
          , "partial"
          , "prelude"
          , "tuples"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-bignumber"
        , version = "760d11b41ece31b8cdd3c53349c5c2fd48d3ff89"
        }
      , properties =
        { dependencies = [ "prelude", "console" ]
        , repo = "https://github.com/Risto-Stevcev/purescript-properties.git"
        , version = "v0.2.0"
        }
      , lattice =
        { dependencies = [ "prelude", "console", "properties" ]
        , repo = "https://github.com/Risto-Stevcev/purescript-lattice.git"
        , version = "v0.3.0"
        }
      , mote =
        { dependencies = [ "these", "transformers", "arrays" ]
        , repo = "https://github.com/garyb/purescript-mote"
        , version = "v1.1.0"
        }
      , toppokki =
        { dependencies =
          [ "prelude"
          , "record"
          , "functions"
          , "node-http"
          , "aff-promise"
          , "node-buffer"
          , "node-fs-aff"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-toppokki"
        , version = "5992e93396a734c980ef61c74df5b6ab46108920"
        }
      , noble-secp256k1 =
        { dependencies =
          [ "aff"
          , "aff-promise"
          , "bytearrays"
          , "effect"
          , "prelude"
          , "spec"
          , "tuples"
          , "unsafe-coerce"
          ]
        , repo =
            "https://github.com/mlabs-haskell/purescript-noble-secp256k1.git"
        , version = "v2.0.0"
        }
      , js-bigints =
        { dependencies = [ "integers", "maybe", "prelude" ]
        , repo = "https://github.com/purescript-contrib/purescript-js-bigints"
        , version = "36a7d8ac75a7230043ae511f3145f9ed130954a9"
        }
      , cip30 =
        { dependencies =
          [ "aff"
          , "aff-promise"
          , "arrays"
          , "console"
          , "effect"
          , "literals"
          , "maybe"
          , "newtype"
          , "nullable"
          , "prelude"
          , "untagged-union"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-cip30"
        , version = "8f1b34b48825fcec5e9c67f33e255770b1e0bc45"
        }
      , cip30-typesafe =
        { dependencies =
          [ "aff"
          , "bifunctors"
          , "cip30"
          , "control"
          , "effect"
          , "either"
          , "exceptions"
          , "maybe"
          , "prelude"
          , "spec"
          , "transformers"
          , "variant"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-cip30-typesafe"
        , version = "d72e51fbc0255eb3246c9132d295de7f65e16a99"
        }
      , bytearrays =
        { dependencies =
          [ "aeson"
          , "aff"
          , "arraybuffer-types"
          , "effect"
          , "either"
          , "foldable-traversable"
          , "maybe"
          , "newtype"
          , "prelude"
          , "quickcheck"
          , "quickcheck-laws"
          , "spec"
          , "strings"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-bytearrays"
        , version = "v1.0.0"
        }
      , cardano-serialization-lib =
        { dependencies =
          [ "aeson"
          , "argonaut"
          , "bifunctors"
          , "bytearrays"
          , "effect"
          , "either"
          , "maybe"
          , "nullable"
          , "ordered-collections"
          , "prelude"
          , "profunctor"
          , "tuples"
          ]
        , repo =
            "https://github.com/mlabs-haskell/purescript-cardano-serialization-lib"
        , version = "26d437c3f398172c839b93b57c30eb43a7fa7ca0"
        }
      , cardano-plutus-data-schema =
        { dependencies = [ "prelude" ]
        , repo =
            "https://github.com/mlabs-haskell/purescript-cardano-plutus-data-schema"
        , version = "v1.0.0"
        }
      , plutus-types =
        { dependencies =
          [ "aeson"
          , "argonaut-codecs"
          , "arrays"
          , "bifunctors"
          , "bytearrays"
          , "cardano-plutus-data-schema"
          , "cardano-types"
          , "console"
          , "effect"
          , "either"
          , "foldable-traversable"
          , "gen"
          , "js-bigints"
          , "lattice"
          , "maybe"
          , "monad-logger"
          , "newtype"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "profunctor-lenses"
          , "quickcheck"
          , "these"
          , "tuples"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-plutus-types"
        , version = "v1.0.1"
        }
      , cardano-types =
        { dependencies =
          [ "aeson"
          , "arraybuffer-types"
          , "arrays"
          , "bifunctors"
          , "bytearrays"
          , "cardano-plutus-data-schema"
          , "cardano-serialization-lib"
          , "console"
          , "control"
          , "effect"
          , "either"
          , "encoding"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "integers"
          , "js-bigints"
          , "lattice"
          , "lists"
          , "literals"
          , "maybe"
          , "monad-logger"
          , "newtype"
          , "nonempty"
          , "nullable"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "profunctor"
          , "quickcheck"
          , "rationals"
          , "record"
          , "safe-coerce"
          , "strings"
          , "these"
          , "tuples"
          , "typelevel-prelude"
          , "uint"
          , "unfoldable"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-cardano-types"
        , version = "40d9468a4712ad2bf57ebede19fae92208f082a0"
        }
      , cardano-message-signing =
        { dependencies =
          [ "bytearrays"
          , "cardano-types"
          , "console"
          , "effect"
          , "newtype"
          , "prelude"
          ]
        , repo =
            "https://github.com/mlabs-haskell/purescript-cardano-message-signing"
        , version = "v1.0.0"
        }
      , cardano-hd-wallet =
        { dependencies =
          [ "cardano-serialization-lib"
          , "cardano-types"
          , "console"
          , "effect"
          , "either"
          , "prelude"
          , "uint"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-cardano-hd-wallet"
        , version = "v1.0.0"
        }
      , mote-testplan =
        { dependencies =
          [ "aff"
          , "console"
          , "datetime"
          , "effect"
          , "foldable-traversable"
          , "maybe"
          , "mote"
          , "newtype"
          , "numbers"
          , "ordered-collections"
          , "prelude"
          , "spec"
          , "transformers"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-mote-testplan"
        , version = "v1.0.0"
        }
      }

in  upstream // additions
