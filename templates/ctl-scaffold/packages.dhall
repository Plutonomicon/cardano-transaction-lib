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
        , version = "v2.0.0"
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
          , "effect"
          , "prelude"
          , "spec"
          , "tuples"
          , "unsafe-coerce"
          ]
        , repo =
            "https://github.com/mlabs-haskell/purescript-noble-secp256k1.git"
        , version = "a3c0f67e9fdb0086016d7aebfad35d09a08b4ecd"
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
      , cardano-transaction-lib =
        { dependencies =
          [ "aeson"
          , "aff"
          , "aff-promise"
          , "aff-retry"
          , "affjax"
          , "ansi"
          , "argonaut"
          , "argonaut-codecs"
          , "arraybuffer-types"
          , "arrays"
          , "avar"
          , "bifunctors"
          , "bignumber"
          , "checked-exceptions"
          , "cip30"
          , "cip30-typesafe"
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
          , "js-bigints"
          , "js-date"
          , "lattice"
          , "lists"
          , "maybe"
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
          , "toppokki"
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
        , version = "63485e328b6008b5bea336269b9d6036d04c7c7b"
        }
      , errors =
        { dependencies =
          [ "control"
          , "effect"
          , "either"
          , "identity"
          , "maybe"
          , "newtype"
          , "prelude"
          , "test-unit"
          , "transformers"
          ]
        , repo = "https://github.com/passy/purescript-errors.git"
        , version = "670485beb1e026f77d52ca58ce10c145d96c11ba"
        }
      , ply-ctl =
        { dependencies =
          [ "effect"
          , "prelude"
          , "cardano-transaction-lib"
          , "bigints"
          , "aeson"
          , "either"
          , "newtype"
          , "node-buffer"
          , "node-fs"
          , "tuples"
          , "arrays"
          , "uint"
          , "node-process"
          , "integers"
          ]
        , repo = "https://github.com/mlabs-haskell/ply-ctl.git"
        , version = "727b811b0d561cf13d5594b9352a7294e5a20378"
        }
      }

in  (upstream // additions)
