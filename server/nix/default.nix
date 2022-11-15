{ src
, inputs
, pkgs
, doCoverage ? false
, deferPluginErrors ? true
, CHaP
, ...
}:

pkgs.haskell-nix.cabalProject {
  inherit src;

  name = "ctl-server";

  inputMap = {
    "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
  };

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
          pkgs.lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
      };
    }
  ];
}
