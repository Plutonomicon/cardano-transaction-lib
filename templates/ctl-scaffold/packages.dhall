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
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20220224/packages.dhall
        sha256:67cc3d4f0e8fb72bb1413ba94ddd72a3ceb0783eb725e3b22ad7568b3b581163

let additions =
      { aeson =
        { dependencies =
          [ "aff"
          , "argonaut"
          , "argonaut-codecs"
          , "argonaut-core"
          , "arrays"
          , "bifunctors"
          , "bigints"
          , "bignumber"
          , "const"
          , "control"
          , "effect"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "integers"
          , "lists"
          , "maybe"
          , "mote"
          , "numbers"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "quickcheck"
          , "record"
          , "sequences"
          , "spec"
          , "strings"
          , "tuples"
          , "typelevel"
          , "typelevel-prelude"
          , "uint"
          , "untagged-union"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-aeson.git"
        , version = "bfd8f4dcd0522a076320f9dc710c24817438e02e"
        }
      , sequences =
        { dependencies =
          [ "arrays"
          , "assert"
          , "console"
          , "effect"
          , "lazy"
          , "maybe"
          , "newtype"
          , "nonempty"
          , "partial"
          , "prelude"
          , "profunctor"
          , "psci-support"
          , "quickcheck"
          , "quickcheck-laws"
          , "tuples"
          , "unfoldable"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/hdgarrood/purescript-sequences"
        , version = "v3.0.2"
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
      , medea =
        { dependencies =
          [ "aff"
          , "argonaut"
          , "arrays"
          , "bifunctors"
          , "control"
          , "effect"
          , "either"
          , "enums"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "free"
          , "integers"
          , "lists"
          , "maybe"
          , "mote"
          , "naturals"
          , "newtype"
          , "node-buffer"
          , "node-fs-aff"
          , "node-path"
          , "nonempty"
          , "ordered-collections"
          , "parsing"
          , "partial"
          , "prelude"
          , "psci-support"
          , "quickcheck"
          , "quickcheck-combinators"
          , "safely"
          , "spec"
          , "strings"
          , "these"
          , "transformers"
          , "typelevel"
          , "tuples"
          , "unicode"
          , "unordered-collections"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/juspay/medea-ps.git"
        , version = "8b215851959aa8bbf33e6708df6bd683c89d1a5a"
        }
      , purescript-toppokki =
        { dependencies =
          [ "prelude"
          , "record"
          , "functions"
          , "node-http"
          , "aff-promise"
          , "node-buffer"
          , "node-fs-aff"
          ]
        , repo = "https://github.com/firefrorefiddle/purescript-toppokki"
        , version = "6983e07bf0aa55ab779bcef12df3df339a2b5bd9"
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
        , version = "705923edd892a3397b90d28ce7db9a7181dcd599"
        }
      , cardano-transaction-lib =
        { dependencies =
          [ "aeson"
          , "argonaut-codecs"
          , "aff"
          , "aff-promise"
          , "aff-retry"
          , "affjax"
          , "argonaut"
          , "arraybuffer-types"
          , "arrays"
          , "avar"
          , "bifunctors"
          , "bigints"
          , "checked-exceptions"
          , "console"
          , "control"
          , "datetime"
          , "debug"
          , "effect"
          , "either"
          , "encoding"
          , "enums"
          , "exceptions"
          , "foldable-traversable"
          , "foreign"
          , "foreign-object"
          , "functions"
          , "gen"
          , "heterogeneous"
          , "http-methods"
          , "identity"
          , "integers"
          , "js-date"
          , "lattice"
          , "lists"
          , "math"
          , "maybe"
          , "medea"
          , "media-types"
          , "monad-logger"
          , "mote"
          , "newtype"
          , "noble-secp256k1"
          , "node-buffer"
          , "node-child-process"
          , "node-fs"
          , "node-fs-aff"
          , "node-path"
          , "node-process"
          , "node-readline"
          , "node-streams"
          , "nonempty"
          , "now"
          , "numbers"
          , "optparse"
          , "ordered-collections"
          , "orders"
          , "parallel"
          , "partial"
          , "posix-types"
          , "prelude"
          , "profunctor"
          , "profunctor-lenses"
          , "purescript-toppokki"
          , "quickcheck"
          , "quickcheck-combinators"
          , "quickcheck-laws"
          , "rationals"
          , "record"
          , "refs"
          , "safe-coerce"
          , "spec"
          , "spec-quickcheck"
          , "strings"
          , "stringutils"
          , "tailrec"
          , "these"
          , "transformers"
          , "tuples"
          , "typelevel"
          , "typelevel-prelude"
          , "uint"
          , "undefined"
          , "unfoldable"
          , "untagged-union"
          , "variant"
          ]
        , repo = "https://github.com/Plutonomicon/cardano-transaction-lib.git"
        , version = "27f997461fda4a6f7eb52f1165a91d7d453fb990"
        }
      , noble-secp256k1 =
        { dependencies =
          [ "aff"
          , "aff-promise"
          , "effect"
          , "prelude"
          , "spec"
          , "tuples"
          , "unsafe-coerce"
          ]
        , repo =
            "https://github.com/mlabs-haskell/purescript-noble-secp256k1.git"
        , version = "710c15c48c5afae5e0623664d982a587ff2bd177"
        }
      }

in  (upstream // additions)
  with parsing.version = "v7.0.1"
