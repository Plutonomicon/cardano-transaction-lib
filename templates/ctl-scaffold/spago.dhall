{ name = "ctl-scaffold"
, dependencies =
    [ "cardano-transaction-lib"
    , "aff-promise"
    , "maybe"
    , "newtype"
    , "prelude"
    ]
, packages = ./packages.dhall
, sources =
    [ "src/**/*.purs"
    ]
}
