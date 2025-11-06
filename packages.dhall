let upstream =
    -- https://github.com/mlabs-haskell/purescript-cardano-package-set
      https://raw.githubusercontent.com/mlabs-haskell/purescript-cardano-package-set/v3.1.0/packages.dhall
        sha256:0d8a7ca4e8ecfc8d1d795a989b76364caa9583d60e765c490cfa215a8824c246

let additions =
      { cardano-kupmios-provider =
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
        , repo = "https://github.com/mlabs-haskell/purescript-cardano-kupmios-provider"
        , version = "a96b25fb05e52295e42578539939e170c8ab8530" 
        }
      }

in (upstream // additions)
