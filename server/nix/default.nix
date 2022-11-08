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
      src = inputs.ogmios.inputs.cardano-base;
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
      src = inputs.ogmios.inputs.cardano-crypto;
      subdirs = [
        "."
      ];
    }
    {
      src = inputs.ogmios.inputs.cardano-ledger;
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
        "libs/cardano-data"
        "libs/cardano-ledger-core"
        "libs/cardano-ledger-pretty"
        "libs/cardano-protocol-tpraos"
        "libs/non-integral"
        "libs/set-algebra"
        "libs/small-steps"
        "libs/small-steps-test"
        "libs/vector-map"
      ];
    }
    {
      src = inputs.ogmios.inputs.cardano-node;
      subdirs = [
        "cardano-api"
      ];
    }
    {
      src = inputs.ogmios.inputs.cardano-prelude;
      subdirs = [
        "cardano-prelude"
        "cardano-prelude-test"
      ];
    }
    {
      src = inputs.ogmios.inputs.ekg-json;
      subdirs = [
        "."
      ];
    }
    {
      src = inputs.ogmios.inputs.flat;
      subdirs = [
        "."
      ];
    }
    {
      src = inputs.ogmios.inputs.goblins;
      subdirs = [
        "."
      ];
    }
    {
      src = inputs.ogmios.inputs.hedgehog-extras;
      subdirs = [
        "."
      ];
    }
    {
      src = inputs.ogmios.inputs.io-sim;
      subdirs = [
        "io-classes"
        "io-sim"
        "strict-stm"
      ];
    }
    {
      src = inputs.ogmios.inputs.iohk-monitoring-framework;
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
      src = inputs.ogmios.inputs.optparse-applicative;
      subdirs = [
        "."
      ];
    }
    {
      src = inputs.ogmios.inputs.ouroboros-network;
      subdirs = [
        "monoidal-synchronisation"
        "network-mux"
        "ouroboros-consensus"
        "ouroboros-consensus-test"
        "ouroboros-consensus-byron"
        "ouroboros-consensus-byronspec"
        "ouroboros-consensus-byron-test"
        "ouroboros-consensus-cardano"
        "ouroboros-consensus-protocol"
        "ouroboros-consensus-shelley"
        "ouroboros-consensus-shelley-test"
        "ouroboros-consensus-cardano-test"
        "ouroboros-network"
        "ouroboros-network-framework"
        "ouroboros-network-testing"
      ];
    }
    {
      src = inputs.ogmios.inputs.plutus;
      subdirs = [
        "plutus-core"
        "plutus-ledger-api"
        "plutus-tx"
        "prettyprinter-configurable"
        "stubs/plutus-ghc-stub"
        "word-array"
      ];
    }
    {
      src = inputs.ogmios.inputs.typed-protocols;
      subdirs = [
        "typed-protocols"
        "typed-protocols-cborg"
        "typed-protocols-examples"
      ];
    }
    {
      src = inputs.ogmios.inputs.Win32-network;
      subdirs = [
        "."
      ];
    }

  ];
}
