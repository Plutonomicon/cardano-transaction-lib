{ src
, inputs
, pkgs
, doCoverage ? false
, deferPluginErrors ? true
, ...
}:

pkgs.haskell-nix.cabalProject {
  inherit src;

  name = "cardano-browser-tx-server";

  compiler-nix-name = "ghc8107";

  shell = {
    inputsFrom = [ pkgs.libsodium-vrf ];

    # Make sure to keep this list updated after upgrading git dependencies!
    additional = ps: with ps; [
      filemanip
      ieee
      cardano-api
      cardano-binary
      cardano-ledger-shelley
      cardano-ledger-shelley-ma
      cardano-prelude
      plutus-tx
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
        # see https://github.com/input-output-hk/haskell.nix/issues/1128
        ieee.components.library.libs = pkgs.lib.mkForce [ ];

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
      src = inputs.cardano-crypto;
      subdirs = [
        "."
      ];
    }
    {
      src = inputs.cardano-ledger-specs;
      subdirs = [
        "byron/ledger/impl"
        "cardano-ledger-core"
        "cardano-protocol-tpraos"
        "eras/alonzo/impl"
        "eras/byron/chain/executable-spec"
        "eras/byron/crypto"
        "eras/byron/crypto/test"
        "eras/byron/ledger/executable-spec"
        "eras/byron/ledger/impl/test"
        "eras/shelley/impl"
        "eras/shelley-ma/impl"
        "eras/shelley/chain-and-ledger/executable-spec"
        "eras/shelley/test-suite"
        "shelley/chain-and-ledger/shelley-spec-ledger-test"
        "libs/non-integral"
        "libs/small-steps"
        "libs/cardano-ledger-pretty"
        "semantics/small-steps-test"
      ];
    }
    {
      src = inputs.cardano-node;
      subdirs = [
        "cardano-api"
        "cardano-node"
        "cardano-cli"
        "cardano-config"
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
        "lib/launcher"
        "lib/core-integration"
        "lib/cli"
        "lib/shelley"
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
        "iohk-monitoring"
        "tracer-transformers"
        "contra-tracer"
        "plugins/backend-aggregation"
        "plugins/backend-ekg"
        "plugins/backend-monitoring"
        "plugins/backend-trace-forwarder"
        "plugins/scribe-systemd"
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
        "typed-protocols"
        "typed-protocols-cborg"
        "typed-protocols-examples"
        "ouroboros-network"
        "ouroboros-network-testing"
        "ouroboros-network-framework"
        "ouroboros-consensus"
        "ouroboros-consensus-byron"
        "ouroboros-consensus-cardano"
        "ouroboros-consensus-shelley"
        "io-sim"
        "io-classes"
        "network-mux"
        "ntp-client"
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
