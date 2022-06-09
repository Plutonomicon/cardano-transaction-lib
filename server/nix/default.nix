{ src
, inputs
, pkgs
, doCoverage ? false
, deferPluginErrors ? true
, ...
}:

pkgs.haskell-nix.cabalProject {
  inherit src;

  name = "ctl-server";

  compiler-nix-name = "ghc8107";

  shell = {
    inputsFrom = [ pkgs.libsodium-vrf ];

    # Make sure to keep this list updated after upgrading git dependencies!
    additional = ps: with ps; [
      cardano-api
      cardano-binary
      cardano-ledger-shelley
      cardano-ledger-shelley-ma
      cardano-prelude
      optparse-applicative
      plutus-tx
      plutus-ledger-api
    ];

    withHoogle = true;

    tools = {
      cabal = "latest";
      haskell-language-server = "latest";
    };

    exactDeps = true;

    nativeBuildInputs = with pkgs;
      [
        haskellPackages.fourmolu
        hlint
        git
        libsodium-vrf
      ];
  };


  modules = [
    {
      packages = {
        cardano-crypto-praos.components.library.pkgconfig =
          pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
        cardano-crypto-class.components.library.pkgconfig =
          pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
      };
    }
  ];

  extraSources = [
    {
      src = inputs.cardano-addresses;
      subdirs = [
        "core"
        "command-line"
      ];
    }
    {
      src = inputs.cardano-base;
      subdirs = [
        "base-deriving-via"
        "binary"
        "binary/test"
        "cardano-crypto-class"
        "cardano-crypto-praos"
        "cardano-crypto-tests"
        "measures"
        "orphans-deriving-via"
        "slotting"
        "strict-containers"
      ];
    }
    {
      src = inputs.cardano-config;
      subdirs = [
        "."
      ];
    }
    {
      src = inputs.cardano-crypto;
      subdirs = [
        "."
      ];
    }
    {
      src = inputs.cardano-ledger;
      subdirs = [
        "eras/alonzo/impl"
        "eras/alonzo/test-suite"
        "eras/babbage/impl"
        "eras/babbage/test-suite"
        "eras/byron/chain/executable-spec"
        "eras/byron/crypto"
        "eras/byron/crypto/test"
        "eras/byron/ledger/executable-spec"
        "eras/byron/ledger/impl"
        "eras/byron/ledger/impl/test"
        "eras/shelley/impl"
        "eras/shelley/test-suite"
        "eras/shelley-ma/impl"
        "eras/shelley-ma/test-suite"
        "libs/cardano-ledger-core"
        "libs/cardano-ledger-pretty"
        "libs/cardano-protocol-tpraos"
        "libs/cardano-data"
        "libs/set-algebra"
        "libs/small-steps"
        "libs/small-steps-test"
        "libs/non-integral"
        "libs/vector-map"
      ];
    }
    {
      src = inputs.cardano-node;
      subdirs = [
        "cardano-api"
        "cardano-cli"
        "cardano-node"
        "cardano-node-capi"
        "cardano-node-chairman"
        "cardano-git-rev"
        "cardano-submit-api"
        "bench/cardano-topology"
        "bench/locli"
        "bench/tx-generator"
        "trace-dispatcher"
        "trace-resources"
        "trace-forward"
      ];
    }
    {
      src = inputs.cardano-prelude;
      subdirs = [
        "cardano-prelude"
        "cardano-prelude-test"
      ];
    }
    {
      src = inputs.cardano-wallet;
      subdirs = [
        "lib/text-class"
        "lib/strict-non-empty-containers"
        "lib/core"
        "lib/test-utils"
        "lib/numeric"
      ];
    }
    {
      src = inputs.ekg-forward;
      subdirs = [
        "."
      ];
    }
    {
      src = inputs.flat;
      subdirs = [
        "."
      ];
    }
    {
      src = inputs.goblins;
      subdirs = [
        "."
      ];
    }
    {
      src = inputs.iohk-monitoring-framework;
      subdirs = [
        "contra-tracer"
        "iohk-monitoring"
        "plugins/backend-aggregation"
        "plugins/backend-ekg"
        "plugins/backend-monitoring"
        "plugins/backend-trace-forwarder"
        "plugins/scribe-systemd"
        "tracer-transformers"
      ];
    }
    {
      src = inputs.optparse-applicative;
      subdirs = [
        "."
      ];
    }
    {
      src = inputs.ouroboros-network;
      subdirs = [
        "monoidal-synchronisation"
        "network-mux"
        "ntp-client"
        "ouroboros-consensus"
        "ouroboros-consensus-byron"
        "ouroboros-consensus-cardano"
        "ouroboros-consensus-protocol"
        "ouroboros-consensus-shelley"
        "ouroboros-network"
        "ouroboros-network-framework"
        "ouroboros-network-testing"
      ];
    }
    {
      src = inputs.typed-protocols;
      subdirs = [
        "typed-protocols"
        "typed-protocols-cborg"
        "typed-protocols-examples"
      ];
    }
    {
      src = inputs.io-sim;
      subdirs = [
        "io-sim"
        "io-classes"
        "strict-stm"
      ];
    }
    {
      src = inputs.plutus;
      subdirs = [
        "plutus-core"
        "plutus-ledger-api"
        "plutus-tx"
        "plutus-tx-plugin"
        "word-array"
        "prettyprinter-configurable"
        "stubs/plutus-ghc-stub"
      ];
    }
    {
      src = inputs.purescript-bridge;
      subdirs = [
        "."
      ];
    }
    {
      src = inputs.servant-purescript;
      subdirs = [
        "."
      ];
    }
    {
      src = inputs.Win32-network;
      subdirs = [
        "."
      ];
    }
  ];
}
