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
        , repo = "https://github.com/errfrom/purescript-aeson.git"
        , version = "f092a1fc9b0ecc2445accf5bf0aad10e6eade89b"
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
        , repo = "https://github.com/garganscript/purescript-sequences"
        , version = "cae456c1a7463785ad33981a93e7a9cb5fc7872c"
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
        , repo = "https://github.com/errfrom/medea-ps.git"
        , version = "00981e4ce7249808413a6db8d88d849bbe85245a"
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
        , repo = "https://github.com/errfrom/purescript-toppokki"
        , version = "b043e9342463df76972d05981ac4ec25316834bf"
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
        , repo = "https://github.com/errfrom/purescript-bignumber"
        , version = "9b3179ad07428d189e42a7a205aab9c7c4849d4a"
        }
      , cardano-transaction-lib =
        { dependencies =
          [ "aeson"
          , "aff"
          , "aff-promise"
          , "aff-retry"
          , "affjax"
          , "argonaut"
          , "argonaut-codecs"
          , "arraybuffer-types"
          , "arrays"
          , "avar"
          , "bifunctors"
          , "bigints"
          , "bignumber"
          , "checked-exceptions"
          , "console"
          , "control"
          , "crypto"
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
          , "formatters"
          , "functions"
          , "gen"
          , "heterogeneous"
          , "http-methods"
          , "identity"
          , "integers"
          , "js-date"
          , "lattice"
          , "lists"
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
          , "random"
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
          , "unfoldable"
          , "untagged-union"
          , "variant"
          , "web-html"
          , "web-storage"
          ]
        , repo = "https://github.com/Plutonomicon/cardano-transaction-lib.git"
        , version = "ef1441b9a401586c8ecfb3d8cdbbb42d4bcda3ea"
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
            "https://github.com/errfrom/purescript-noble-secp256k1.git"
        , version = "9fc5db67e2b6e8b027c415e4dd1625c3f1d8b623"
        }
      }

in  (upstream // additions)
