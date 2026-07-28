let upstream =
    -- https://github.com/mlabs-haskell/purescript-cardano-package-set
      https://raw.githubusercontent.com/mlabs-haskell/purescript-cardano-package-set/v4.1.0/packages.dhall
        sha256:f8d7c3ff5aea758f64cd0876e5be3f8b778b447579b615f143c4043744bd5e37

let additions =
      { cardano-blockfrost-provider =
        { dependencies =
          [ "aeson"
          , "aff"
          , "affjax"
          , "arrays"
          , "bifunctors"
          , "bignumber"
          , "bytearrays"
          , "cardano-data-lite"
          , "cardano-provider"
          , "cardano-types"
          , "datetime"
          , "effect"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "http-methods"
          , "js-bigints"
          , "js-date"
          , "maybe"
          , "media-types"
          , "monad-logger"
          , "newtype"
          , "numbers"
          , "ordered-collections"
          , "parallel"
          , "partial"
          , "prelude"
          , "strings"
          , "transformers"
          , "tuples"
          , "uint"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-cardano-blockfrost-provider"
        , version = "v2.3.0"
        }
      , cardano-kupmios-provider =
        { dependencies =
          [ "aeson"
          , "aff"
          , "affjax"
          , "arrays"
          , "bifunctors"
          , "bytearrays"
          , "cardano-provider"
          , "cardano-data-lite"
          , "cardano-types"
          , "concurrent-queues"
          , "console"
          , "control"
          , "datetime"
          , "effect"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "formatters"
          , "http-methods"
          , "integers"
          , "js-bigints"
          , "js-date"
          , "maybe"
          , "monad-logger"
          , "newtype"
          , "ordered-collections"
          , "parallel"
          , "partial"
          , "prelude"
          , "profunctor-lenses"
          , "record"
          , "strings"
          , "stringutils"
          , "tailrec"
          , "transformers"
          , "tuples"
          , "uint"
          , "untagged-union"
          ]
        , repo =
            "https://github.com/mlabs-haskell/purescript-cardano-kupmios-provider"
        , version = "942af07184e7c03513ad04052209d530bc0ad880"
        }
      }

in (upstream // additions)
